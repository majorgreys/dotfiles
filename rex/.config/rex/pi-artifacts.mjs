// Resolve the focused pane's verified Pi context, never another recent session.
import { stat } from 'node:fs/promises';
import { homedir } from 'node:os';
import { join, resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

export async function startDirectory(record, inspect = stat) {
  const directory = record.sessionFile.slice(0, -'.jsonl'.length) + '/artifacts';
  try {
    if (!(await inspect(directory)).isDirectory()) throw new Error('Pi artifacts path is not a directory.');
    return directory;
  } catch (error) {
    // A verified conversation without saved artifacts can still browse its cwd.
    // Permission errors and malformed paths must not be treated as absence.
    if (error.code === 'ENOENT') return record.cwd;
    throw error;
  }
}

export async function resolvePiDirectory(session, block) {
  if (!/^session:[a-z0-9-]+$/i.test(session ?? '') || !/^block:[a-z0-9-]+$/i.test(block ?? '')) {
    throw new Error('Focus the Pi conversation pane before opening its files.');
  }
  const { readJson, validateRecord, validateConversation, belongsToPane, Rex } = await import(
    pathToFileURL(join(homedir(), 'dev/rex-agent-recovery/recovery.mjs')).href);
  const registration = join(homedir(), '.pi/agent/sessions/.rex-agent-recovery/live', `${block}.json`);
  const record = validateRecord(await readJson(registration));
  const current = value => value.session === session && value.block === block
    && Number.isFinite(value.updatedAt) && Math.abs(Date.now() - value.updatedAt) <= 15000;
  if (!current(record)) throw new Error('No current Pi registration for this pane.');

  const rex = new Rex(join(homedir(), 'Library/Application Support/com.superlogical.rex/RexServer/Versions/Current/Rex Server.app/Contents/MacOS/rex'));
  await rex.identity();
  if (!await belongsToPane(record, await rex.processes(session, block))) {
    throw new Error('The registered Pi process is no longer in this pane.');
  }
  await validateConversation(record);
  const directory = await startDirectory(record);

  const latest = validateRecord(await readJson(registration));
  if (!current(latest) || latest.instance !== record.instance || latest.pid !== record.pid
      || latest.sessionFile !== record.sessionFile || latest.sessionId !== record.sessionId) {
    throw new Error('Pi changed conversations while resolving files. Try again.');
  }
  return { directory };
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  resolvePiDirectory(...process.argv.slice(2)).then(result => {
    console.log(JSON.stringify(result));
  }).catch(error => {
    console.error(`Open in Emacs: ${error.message}\nNo other conversation was opened.`);
    process.exitCode = 1;
  });
}
