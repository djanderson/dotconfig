"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MI2_LLDB = void 0;
const mi2_1 = require("./mi2");
const ChildProcess = require("child_process");
const path = require("path");
class MI2_LLDB extends mi2_1.MI2 {
    initCommands(target, cwd, attach = false) {
        // We need to account for the possibility of the path type used by the debugger being different
        // than the path type where the extension is running (e.g., SSH from Linux to Windows machine).
        // Since the CWD is expected to be an absolute path in the debugger's environment, we can test
        // that to determine the path type used by the debugger and use the result of that test to
        // select the correct API to check whether the target path is an absolute path.
        const debuggerPath = path.posix.isAbsolute(cwd) ? path.posix : path.win32;
        if (!debuggerPath.isAbsolute(target))
            target = debuggerPath.join(cwd, target);
        const cmds = [
            this.sendCommand("gdb-set target-async on"),
            new Promise(resolve => {
                this.sendCommand("list-features").then(done => {
                    this.features = done.result("features");
                    resolve(undefined);
                }, err => {
                    this.features = [];
                    resolve(undefined);
                });
            })
        ];
        if (!attach)
            cmds.push(this.sendCommand("file-exec-and-symbols \"" + mi2_1.escape(target) + "\""));
        for (let cmd of this.extraCommands) {
            cmds.push(this.sendCliCommand(cmd));
        }
        return cmds;
    }
    attach(cwd, executable, target) {
        return new Promise((resolve, reject) => {
            const args = this.preargs.concat(this.extraargs || []);
            this.process = ChildProcess.spawn(this.application, args, { cwd: cwd, env: this.procEnv });
            this.process.stdout.on("data", this.stdout.bind(this));
            this.process.stderr.on("data", this.stderr.bind(this));
            this.process.on("exit", (() => { this.emit("quit"); }).bind(this));
            this.process.on("error", ((err) => { this.emit("launcherror", err); }).bind(this));
            const promises = this.initCommands(target, cwd, true);
            promises.push(this.sendCommand("file-exec-and-symbols \"" + mi2_1.escape(executable) + "\""));
            promises.push(this.sendCommand("target-attach " + target));
            Promise.all(promises).then(() => {
                this.emit("debug-ready");
                resolve(undefined);
            }, reject);
        });
    }
    setBreakPointCondition(bkptNum, condition) {
        return this.sendCommand("break-condition " + bkptNum + " \"" + mi2_1.escape(condition) + "\" 1");
    }
    goto(filename, line) {
        return new Promise((resolve, reject) => {
            // LLDB parses the file differently than GDB...
            // GDB doesn't allow quoting only the file but only the whole argument
            // LLDB doesn't allow quoting the whole argument but rather only the file
            const target = (filename ? '"' + mi2_1.escape(filename) + '":' : "") + line;
            this.sendCliCommand("jump " + target).then(() => {
                this.emit("step-other", null);
                resolve(true);
            }, reject);
        });
    }
}
exports.MI2_LLDB = MI2_LLDB;
//# sourceMappingURL=mi2lldb.js.map