import assert from 'node:assert/strict';
import test from 'node:test';
import { startDirectory, resolvePiDirectory } from '../.config/rex/pi-artifacts.mjs';

const record = {sessionFile: '/sessions/current.jsonl', cwd: '/project'};

test('existing artifacts directory is the starting point', async () => {
  assert.equal(await startDirectory(record, async path => {
    assert.equal(path, '/sessions/current/artifacts');
    return {isDirectory: () => true};
  }), '/sessions/current/artifacts');
});

test('no artifacts yet uses the verified conversation cwd without creating files', async () => {
  assert.equal(await startDirectory(record, async () => {
    throw Object.assign(new Error('missing'), {code: 'ENOENT'});
  }), '/project');
});

test('access denial is not absence', async () => {
  await assert.rejects(startDirectory(record, async () => {
    throw Object.assign(new Error('denied'), {code: 'EACCES'});
  }), {code: 'EACCES'});
});

test('a non-directory artifact path is an error', async () => {
  await assert.rejects(startDirectory(record, async () => ({isDirectory: () => false})), /not a directory/);
});

test('invalid pane identity fails before loading the recovery controller', async () => {
  await assert.rejects(resolvePiDirectory('wrong', 'block:one'), /Focus the Pi conversation/);
  await assert.rejects(resolvePiDirectory('session:one', '../wrong'), /Focus the Pi conversation/);
});
