/**
 * sketchybar-agent-status — push pi agent state into sketchybar.
 *
 * Mirror of the Claude Code agent-status plugin for pi. Fires the
 * `sketchybar-set-state` helper on lifecycle events so the sketchybar
 * Lua side and the agent-panel TUI see pi sessions identically to
 * Claude Code sessions.
 *
 * Event mapping:
 *   input            → running         + capture last_prompt
 *   agent_end        → needs-attention + capture last_assistant
 *   session_shutdown → clear
 *
 * `input` is preferred over `agent_start` because we get the raw user
 * prompt text. We mark the session running at input time even if some
 * other handler intercepts the message — worst case we briefly show
 * "running" for a no-op input, which is corrected by the next
 * agent_end.
 *
 * The helper expects pi's stdin payload to look like a hook event:
 *   { session_id, cwd, agent: "pi", prompt?, last_assistant? }
 * It writes/merges into the same per-session pin file at
 * $XDG_STATE_HOME/sketchybar/sessions/<id>.json.
 *
 * This file lives in dotfiles at
 *   ~/.dotfiles/agent-status/plugins/agent-status-pi/sketchybar-agent-status.ts
 * and is symlinked into ~/.pi/agent/extensions/ by install.sh.
 */

import { execSync } from "node:child_process";
import { basename } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

interface MinimalCtx {
	cwd: string;
	sessionManager: { getSessionFile(): string | undefined };
}

/**
 * Stable session id used by sketchybar/agent-panel.
 *
 * Pi uses the session file path as its natural identity. We strip the
 * directory + .json suffix so the id is shorter and matches what the
 * user would see in `pi --list-sessions`. Falls back to the pi
 * process pid for ephemeral (no session file) sessions.
 */
function sessionId(ctx: MinimalCtx): string {
	const file = ctx.sessionManager.getSessionFile();
	if (file) {
		return basename(file).replace(/\.json$/i, "");
	}
	return `pi-${process.pid}`;
}

/**
 * Extract a plain-text snippet of the assistant's last message for the
 * panel's "what was it just doing" line. Pi messages can carry mixed
 * content blocks; we concatenate text-typed parts. Anything weird
 * coerces to a JSON string so we still get *something* useful.
 */
function extractAssistantText(message: unknown): string {
	if (!message || typeof message !== "object") return "";
	const m = message as { content?: unknown };
	const c = m.content;
	if (typeof c === "string") return c;
	if (Array.isArray(c)) {
		const parts: string[] = [];
		for (const part of c) {
			if (part && typeof part === "object") {
				const p = part as { type?: string; text?: string };
				if (p.type === "text" && typeof p.text === "string") parts.push(p.text);
			}
		}
		if (parts.length > 0) return parts.join("\n");
	}
	try {
		return JSON.stringify(m).slice(0, 500);
	} catch {
		return "";
	}
}

function fire(state: string, payload: Record<string, unknown>): void {
	try {
		execSync("sketchybar-set-state --tool pi " + state, {
			input: JSON.stringify(payload),
			timeout: 5000,
			stdio: ["pipe", "ignore", "ignore"],
		});
	} catch {
		// Best-effort — sketchybar / helper may not be installed.
	}
}

export default function (pi: ExtensionAPI): void {
	let lastPrompt = "";
	let lastSessionId = "";
	let lastCwd = "";

	pi.on("input", async (event, ctx) => {
		// Skip extension-injected messages so background re-prompts
		// don't masquerade as user activity in the panel.
		if (event.source === "extension") return;

		const sid = sessionId(ctx);
		lastSessionId = sid;
		lastCwd = ctx.cwd;
		const text = typeof event.text === "string" ? event.text : "";
		lastPrompt = text;

		fire("running", {
			session_id: sid,
			cwd: ctx.cwd,
			agent: "pi",
			prompt: text,
		});
	});

	pi.on("agent_end", async (event, ctx) => {
		const sid = sessionId(ctx);
		lastSessionId = sid;
		lastCwd = ctx.cwd;

		// agent_end shape: { message?, ... }. Defensive on either name.
		const evt = event as { message?: unknown; lastMessage?: unknown };
		const lastAssistant = extractAssistantText(evt.message ?? evt.lastMessage);

		fire("needs-attention", {
			session_id: sid,
			cwd: ctx.cwd,
			agent: "pi",
			last_assistant: lastAssistant,
		});
	});

	pi.on("session_shutdown", async (_event, ctx) => {
		// Prefer fresh ctx; fall back to last cached identity since ctx
		// may be partially torn down here.
		let sid: string;
		let cwd: string;
		try {
			sid = sessionId(ctx);
			cwd = ctx.cwd;
		} catch {
			sid = lastSessionId;
			cwd = lastCwd;
		}
		if (!sid) return;
		fire("clear", { session_id: sid, cwd, agent: "pi" });
		lastPrompt = "";
		lastSessionId = "";
		lastCwd = "";
	});
}
