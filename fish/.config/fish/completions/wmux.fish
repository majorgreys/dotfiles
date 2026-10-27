# Local fish completions for wmux, based on its installed "wmux help --json" manifest.
# Command/flag snapshot: refresh when wmux adds commands or flags.
# Workspace names come from local SSH snippets; Tab never runs wmux or SSH.
complete -c wmux -f

# Ignore option values when deciding which subcommand comes next.
function __wmux_positionals
    set -l words (commandline -opc)
    set -l value_pending 0
    for word in $words[2..-1]
        if test "$word" = --
            return 1
        end
        if test $value_pending -eq 1
            set value_pending 0
            continue
        end
        if string match -q -- '--*=*' "$word"
            continue
        end
        if contains -- "$word" --account --base-branch --branch --chat-prompt --check --cmux-limit --context --cursor --data-dir --datacenter --ddtool-gitlab-project-token --ddtool-token --ddtool-vault-datacenter --debounce --delegation-file --exclude-reason --exclude-workspace --file --harness --id --idempotency-key --issue-url --kinds --limit --lines --manifest --mcp-server --message --message-file --model --name --namespace --page --path --poll-interval --prompt --prompt-file --reasoning-effort --remove --replacement-rehearsal --repo --review-prompt --scope-file --script --script-file --source --source-path --story --target-epoch --target-id --timeout --type --uri --value --wait-clear-seconds --workspace
            set value_pending 1
            continue
        end
        if string match -q -- '-*' "$word"
            continue
        end
        printf '%s\n' "$word"
    end
    test $value_pending -eq 0
end

function __wmux_at_path
    set -l words (__wmux_positionals)
    or return 1
    test (count $words) -eq (count $argv)
    or return 1
    if test (count $argv) -eq 0
        return 0
    end
    for i in (seq (count $argv))
        test "$words[$i]" = "$argv[$i]"
        or return 1
    end
end

function __wmux_using_command
    set -l words (commandline -opc)
    test (count $words) -ge 2
    and test "$words[2]" = "$argv[1]"
    and not contains -- -- $words[2..-1]
end

# Match wmux's own discovery: a workspace snippet must declare its literal alias.
function __wmux_workspaces --argument-names suffix
    command python3 -c '
import pathlib
import re
import shlex
import sys

root = pathlib.Path.home() / ".ssh" / "workspaces"
for path in sorted(root.glob("*.config")):
    name = path.stem
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]*", name):
        continue
    try:
        with path.open() as config:
            for line in config:
                host = re.match(r"^\s*Host(?:\s*=\s*|\s+)(.*)$", line, re.I)
                if not host:
                    continue
                try:
                    patterns = shlex.split(host[1], comments=True)
                except ValueError:
                    continue
                if "workspace-" + name in patterns:
                    print(name + sys.argv[1])
                    break
    except (OSError, UnicodeError):
        continue
' "$suffix" 2>/dev/null
end

complete -c wmux -s h -l help -d 'Show help'

complete -c wmux -n __wmux_at_path -a chat -d 'open and develop the WMUX chat manager'
for flag in no-restart json wait done
    complete -c wmux -n '__wmux_using_command chat' -l $flag
end
for flag in timeout limit kinds model
    complete -c wmux -n '__wmux_using_command chat' -l $flag -r
end
for flag in source data-dir
    complete -c wmux -n '__wmux_using_command chat' -l $flag -r -F
end
complete -c wmux -n '__wmux_at_path chat' -a 'web dev reload service send history log status threads board events stop model version serve help'

complete -c wmux -n __wmux_at_path -a list -d 'show known remote tmux sessions'
for flag in json refresh no-cache include-operational
    complete -c wmux -n '__wmux_using_command list' -l $flag
end
for flag in workspace
    complete -c wmux -n '__wmux_using_command list' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a create -d 'create a remote tmux session'
for flag in no-attach
    complete -c wmux -n '__wmux_using_command create' -l $flag
end
for flag in workspace repo base-branch
    complete -c wmux -n '__wmux_using_command create' -l $flag -r
end
for flag in delegation-file
    complete -c wmux -n '__wmux_using_command create' -l $flag -r -F
end

complete -c wmux -n __wmux_at_path -a review -d 'open a PR review session with Codex or Claude and Cursor'
for flag in skip-review
    complete -c wmux -n '__wmux_using_command review' -l $flag
end
for flag in harness model reasoning-effort review-prompt chat-prompt
    complete -c wmux -n '__wmux_using_command review' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a delegate -d 'delegate a command to a new top-level wmux workspace'
for flag in script-stdin prompt-stdin fallback-only all summary json message-stdin
    complete -c wmux -n '__wmux_using_command delegate' -l $flag
end
for flag in repo branch base-branch workspace issue-url script prompt lines message idempotency-key target-id target-epoch model reasoning-effort
    complete -c wmux -n '__wmux_using_command delegate' -l $flag -r
end
for flag in script-file prompt-file message-file
    complete -c wmux -n '__wmux_using_command delegate' -l $flag -r -F
end
complete -c wmux -n '__wmux_at_path delegate' -a 'status history cancel replay sessions output send interrupt delivery'

complete -c wmux -n __wmux_at_path -a validate-workspace-config -d 'verify exported workspace secrets before workspace creation'

complete -c wmux -n __wmux_at_path -a attach -d 'attach to an existing remote tmux session'
for flag in rescue delegated-offline
    complete -c wmux -n '__wmux_using_command attach' -l $flag
end
for flag in workspace target-id target-epoch
    complete -c wmux -n '__wmux_using_command attach' -l $flag -x
end
# A bare workspace is not an attach target: continue with workspace:repo[:branch].
complete -c wmux -n '__wmux_at_path attach' -a '(__wmux_workspaces :)' -d 'SSH workspace; add repo[:branch]'
complete -c wmux -n '__wmux_using_command attach' -l workspace -x -a '(__wmux_workspaces)' -d 'Select SSH workspace'

complete -c wmux -n __wmux_at_path -a manage -d 'proactively manage auth on a destination without attaching'
for flag in json
    complete -c wmux -n '__wmux_using_command manage' -l $flag
end
for flag in workspace remove
    complete -c wmux -n '__wmux_using_command manage' -l $flag -r
end
complete -c wmux -n '__wmux_at_path manage' -a auth

complete -c wmux -n __wmux_at_path -a attach-web-ui -d 'connect workspace yarn dev to localhost; Ctrl-C/Ctrl-D cleans up owned preview resources'

complete -c wmux -n __wmux_at_path -a attach-storybook -d 'open workspace Storybook on localhost; Ctrl-C/Ctrl-D cleans up owned preview resources'
for flag in story
    complete -c wmux -n '__wmux_using_command attach-storybook' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a done -d 'confirm cleanup of the current cmux pane\'s wmux session and registered extras'

complete -c wmux -n __wmux_at_path -a delete -d 'queue deletion of a remote tmux session'
for flag in yes force
    complete -c wmux -n '__wmux_using_command delete' -l $flag
end
for flag in workspace
    complete -c wmux -n '__wmux_using_command delete' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a doctor -d 'inspect local and remote wmux state'
for flag in local-only cmux-log include-operational
    complete -c wmux -n '__wmux_using_command doctor' -l $flag
end
for flag in workspace cmux-limit
    complete -c wmux -n '__wmux_using_command doctor' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a repair -d 'inspect or resolve imported target repair_required conflicts'
for flag in apply json
    complete -c wmux -n '__wmux_using_command repair' -l $flag
end
for flag in workspace source source-path
    complete -c wmux -n '__wmux_using_command repair' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a migrate -d 'inspect or apply one-target imported-target migration into target_server'
for flag in apply json
    complete -c wmux -n '__wmux_using_command migrate' -l $flag
end
for flag in workspace
    complete -c wmux -n '__wmux_using_command migrate' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a auth -d 'inspect managed authentication and start explicit recovery actions'
for flag in pending wait allow-disabled no-repair json google mcp warnings ddtool-gitlab-oauth include-operational all
    complete -c wmux -n '__wmux_using_command auth' -l $flag
end
for flag in workspace check page mcp-server ddtool-vault-datacenter ddtool-token ddtool-gitlab-project-token account
    complete -c wmux -n '__wmux_using_command auth' -l $flag -r
end
complete -c wmux -n '__wmux_at_path auth' -a 'ui state status inspect history simulate-expiry warnings open refresh github-workspace-login secret clear doctor'
complete -c wmux -n '__wmux_at_path auth warnings' -a clear
complete -c wmux -n '__wmux_at_path auth secret' -a set

complete -c wmux -n __wmux_at_path -a configure -d 'edit managed auth and config requirements, including optional Kubernetes onboarding, then sync workspaces'
for flag in json validate workspace-local
    complete -c wmux -n '__wmux_using_command configure' -l $flag
end
for flag in workspace id type value name datacenter context namespace issue-url
    complete -c wmux -n '__wmux_using_command configure' -l $flag -r
end
for flag in file
    complete -c wmux -n '__wmux_using_command configure' -l $flag -r -F
end
complete -c wmux -n '__wmux_at_path configure' -a 'show delegation-focus proxy-profile issue-agent enable disable add remove sync remote'
complete -c wmux -n '__wmux_at_path configure proxy-profile' -a 'enable disable status'
complete -c wmux -n '__wmux_at_path configure issue-agent' -a 'apply show status enable disable remove'

complete -c wmux -n __wmux_at_path -a dotfiles -d 'register and sync selected home dotfiles to workspaces'
for flag in json no-initial-sync
    complete -c wmux -n '__wmux_using_command dotfiles' -l $flag
end
for flag in workspace debounce poll-interval
    complete -c wmux -n '__wmux_using_command dotfiles' -l $flag -r
end
for flag in path
    complete -c wmux -n '__wmux_using_command dotfiles' -l $flag -r -F
end
complete -c wmux -n '__wmux_at_path dotfiles' -a 'add remove list sync watch'

complete -c wmux -n __wmux_at_path -a remote -d 'run explicit remote workspace maintenance actions'
for flag in json
    complete -c wmux -n '__wmux_using_command remote' -l $flag
end
for flag in workspace path
    complete -c wmux -n '__wmux_using_command remote' -l $flag -r
end
complete -c wmux -n '__wmux_at_path remote' -a tmux-update

complete -c wmux -n __wmux_at_path -a mcp -d 'call the laptop agent\'s MCP endpoints and stream their push events'
for flag in uri cursor
    complete -c wmux -n '__wmux_using_command mcp' -l $flag -r
end
complete -c wmux -n '__wmux_at_path mcp' -a 'call tools events'

complete -c wmux -n __wmux_at_path -a restart-agents -d 'explicitly restart managed Codex agents across wmux sessions'
for flag in upgrade-only json
    complete -c wmux -n '__wmux_using_command restart-agents' -l $flag
end
for flag in workspace
    complete -c wmux -n '__wmux_using_command restart-agents' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a upgrade-agents -d 'safely reconcile coding-agent capabilities and auxiliary tools'
for flag in local-only dry-run json
    complete -c wmux -n '__wmux_using_command upgrade-agents' -l $flag
end
for flag in workspace
    complete -c wmux -n '__wmux_using_command upgrade-agents' -l $flag -r
end
for flag in manifest
    complete -c wmux -n '__wmux_using_command upgrade-agents' -l $flag -r -F
end

complete -c wmux -n __wmux_at_path -a reload-active -d 'reexec active local wmux attach processes'
for flag in no-verify plan-only
    complete -c wmux -n '__wmux_using_command reload-active' -l $flag
end
for flag in wait-clear-seconds exclude-workspace exclude-reason
    complete -c wmux -n '__wmux_using_command reload-active' -l $flag -r
end
for flag in scope-file
    complete -c wmux -n '__wmux_using_command reload-active' -l $flag -r -F
end

complete -c wmux -n __wmux_at_path -a refresh-cache -d 'refresh the local workspace/session cache'
for flag in workspace
    complete -c wmux -n '__wmux_using_command refresh-cache' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a version -d 'print the local wmux runtime build identity'

complete -c wmux -n __wmux_at_path -a self-install -d 'install or roll back the local wmux front door'
for flag in rollback
    complete -c wmux -n '__wmux_using_command self-install' -l $flag
end
for flag in replacement-rehearsal
    complete -c wmux -n '__wmux_using_command self-install' -l $flag -r
end

complete -c wmux -n __wmux_at_path -a help -d 'print the command list, detailed help for one command, or the machine-readable manifest'
for flag in json
    complete -c wmux -n '__wmux_using_command help' -l $flag
end

complete -c wmux -n __wmux_at_path -a docs -d 'print the complete wmux handbook from the embedded README and user guide'

complete -c wmux -n __wmux_at_path -a manpage -d 'print the wmux man page in roff format'

# Help accepts a command name rather than a live target.
complete -c wmux -n '__wmux_at_path help; or __wmux_at_path docs' -a 'chat list create review delegate validate-workspace-config attach manage attach-web-ui attach-storybook done delete doctor repair migrate auth configure dotfiles remote mcp restart-agents upgrade-agents reload-active refresh-cache version self-install help docs manpage'
complete -c wmux -n '__wmux_using_command review' -l harness -r -a 'codex claude'
complete -c wmux -n '__wmux_using_command auth' -l account -r -a 'personal managed'
complete -c wmux -n '__wmux_at_path configure delegation-focus' -a 'always never'
