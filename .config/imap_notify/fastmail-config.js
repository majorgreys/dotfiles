var child_process = require('child_process');

function getStdout(cmd) {
    var stdout = child_process.execSync(cmd);
    return stdout.toString().trim();
}

exports.host = "imap.fastmail.com"
exports.port = 993;
exports.tls = true;
exports.tlsOptions = { "rejectUnauthorized": false };
exports.username = "tahir@tahirbutt.com";
exports.password = getStdout("/usr/bin/gpg -q --for-your-eyes-only --no-tty -d ~/.passwords/.fastmail.gpg");
exports.onNotify = "/usr/bin/mbsync -V fastmail"
exports.onNotifyPost = {"mail": "/usr/bin/notmuch new && /usr/bin/afew -nt"}
exports.boxes = [ "INBOX" ];
