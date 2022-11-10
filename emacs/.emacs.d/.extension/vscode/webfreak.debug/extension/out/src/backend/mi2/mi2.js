"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.MI2 = exports.escape = void 0;
const backend_1 = require("../backend");
const ChildProcess = require("child_process");
const events_1 = require("events");
const mi_parse_1 = require("../mi_parse");
const linuxTerm = require("../linux/console");
const net = require("net");
const path = require("path");
const ssh2_1 = require("ssh2");
function escape(str) {
    return str.replace(/\\/g, "\\\\").replace(/"/g, "\\\"");
}
exports.escape = escape;
const nonOutput = /^(?:\d*|undefined)[\*\+\=]|[\~\@\&\^]/;
const gdbMatch = /(?:\d*|undefined)\(gdb\)/;
const numRegex = /\d+/;
function couldBeOutput(line) {
    if (nonOutput.exec(line))
        return false;
    return true;
}
const trace = false;
class MI2 extends events_1.EventEmitter {
    constructor(application, preargs, extraargs, procEnv, extraCommands = []) {
        super();
        this.application = application;
        this.preargs = preargs;
        this.extraargs = extraargs;
        this.extraCommands = extraCommands;
        this.prettyPrint = true;
        this.currentToken = 1;
        this.handlers = {};
        this.breakpoints = new Map();
        if (procEnv) {
            const env = {};
            // Duplicate process.env so we don't override it
            for (const key in process.env)
                if (process.env.hasOwnProperty(key))
                    env[key] = process.env[key];
            // Overwrite with user specified variables
            for (const key in procEnv) {
                if (procEnv.hasOwnProperty(key)) {
                    if (procEnv === null)
                        delete env[key];
                    else
                        env[key] = procEnv[key];
                }
            }
            this.procEnv = env;
        }
    }
    load(cwd, target, procArgs, separateConsole) {
        if (!path.isAbsolute(target))
            target = path.join(cwd, target);
        return new Promise((resolve, reject) => {
            this.isSSH = false;
            const args = this.preargs.concat(this.extraargs || []);
            this.process = ChildProcess.spawn(this.application, args, { cwd: cwd, env: this.procEnv });
            this.process.stdout.on("data", this.stdout.bind(this));
            this.process.stderr.on("data", this.stderr.bind(this));
            this.process.on("exit", (() => { this.emit("quit"); }).bind(this));
            this.process.on("error", ((err) => { this.emit("launcherror", err); }).bind(this));
            const promises = this.initCommands(target, cwd);
            if (procArgs && procArgs.length)
                promises.push(this.sendCommand("exec-arguments " + procArgs));
            if (process.platform == "win32") {
                if (separateConsole !== undefined)
                    promises.push(this.sendCommand("gdb-set new-console on"));
                Promise.all(promises).then(() => {
                    this.emit("debug-ready");
                    resolve(undefined);
                }, reject);
            }
            else {
                if (separateConsole !== undefined) {
                    linuxTerm.spawnTerminalEmulator(separateConsole).then(tty => {
                        promises.push(this.sendCommand("inferior-tty-set " + tty));
                        Promise.all(promises).then(() => {
                            this.emit("debug-ready");
                            resolve(undefined);
                        }, reject);
                    });
                }
                else {
                    Promise.all(promises).then(() => {
                        this.emit("debug-ready");
                        resolve(undefined);
                    }, reject);
                }
            }
        });
    }
    ssh(args, cwd, target, procArgs, separateConsole, attach) {
        return new Promise((resolve, reject) => {
            this.isSSH = true;
            this.sshReady = false;
            this.sshConn = new ssh2_1.Client();
            if (separateConsole !== undefined)
                this.log("stderr", "WARNING: Output to terminal emulators are not supported over SSH");
            if (args.forwardX11) {
                this.sshConn.on("x11", (info, accept, reject) => {
                    const xserversock = new net.Socket();
                    xserversock.on("error", (err) => {
                        this.log("stderr", "Could not connect to local X11 server! Did you enable it in your display manager?\n" + err);
                    });
                    xserversock.on("connect", () => {
                        const xclientsock = accept();
                        xclientsock.pipe(xserversock).pipe(xclientsock);
                    });
                    xserversock.connect(args.x11port, args.x11host);
                });
            }
            const connectionArgs = {
                host: args.host,
                port: args.port,
                username: args.user
            };
            if (args.useAgent) {
                connectionArgs.agent = process.env.SSH_AUTH_SOCK;
            }
            else if (args.keyfile) {
                if (require("fs").existsSync(args.keyfile))
                    connectionArgs.privateKey = require("fs").readFileSync(args.keyfile);
                else {
                    this.log("stderr", "SSH key file does not exist!");
                    this.emit("quit");
                    reject();
                    return;
                }
            }
            else {
                connectionArgs.password = args.password;
            }
            this.sshConn.on("ready", () => {
                this.log("stdout", "Running " + this.application + " over ssh...");
                const execArgs = {};
                if (args.forwardX11) {
                    execArgs.x11 = {
                        single: false,
                        screen: args.remotex11screen
                    };
                }
                let sshCMD = this.application + " " + this.preargs.concat(this.extraargs || []).join(" ");
                if (args.bootstrap)
                    sshCMD = args.bootstrap + " && " + sshCMD;
                this.sshConn.exec(sshCMD, execArgs, (err, stream) => {
                    if (err) {
                        this.log("stderr", "Could not run " + this.application + "(" + sshCMD + ") over ssh!");
                        if (err === undefined) {
                            err = "<reason unknown>";
                        }
                        this.log("stderr", err.toString());
                        this.emit("quit");
                        reject();
                        return;
                    }
                    this.sshReady = true;
                    this.stream = stream;
                    stream.on("data", this.stdout.bind(this));
                    stream.stderr.on("data", this.stderr.bind(this));
                    stream.on("exit", (() => {
                        this.emit("quit");
                        this.sshConn.end();
                    }).bind(this));
                    const promises = this.initCommands(target, cwd, attach);
                    promises.push(this.sendCommand("environment-cd \"" + escape(cwd) + "\""));
                    if (attach) {
                        // Attach to local process
                        promises.push(this.sendCommand("target-attach " + target));
                    }
                    else if (procArgs && procArgs.length)
                        promises.push(this.sendCommand("exec-arguments " + procArgs));
                    Promise.all(promises).then(() => {
                        this.emit("debug-ready");
                        resolve(undefined);
                    }, reject);
                });
            }).on("error", (err) => {
                this.log("stderr", "Error running " + this.application + " over ssh!");
                if (err === undefined) {
                    err = "<reason unknown>";
                }
                this.log("stderr", err.toString());
                this.emit("quit");
                reject();
            }).connect(connectionArgs);
        });
    }
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
            this.sendCommand("gdb-set target-async on", true),
            new Promise(resolve => {
                this.sendCommand("list-features").then(done => {
                    this.features = done.result("features");
                    resolve(undefined);
                }, () => {
                    // Default to no supported features on error
                    this.features = [];
                    resolve(undefined);
                });
            }),
            this.sendCommand("environment-directory \"" + escape(cwd) + "\"", true)
        ];
        if (!attach)
            cmds.push(this.sendCommand("file-exec-and-symbols \"" + escape(target) + "\""));
        if (this.prettyPrint)
            cmds.push(this.sendCommand("enable-pretty-printing"));
        for (let cmd of this.extraCommands) {
            cmds.push(this.sendCommand(cmd));
        }
        return cmds;
    }
    attach(cwd, executable, target) {
        return new Promise((resolve, reject) => {
            let args = [];
            if (executable && !path.isAbsolute(executable))
                executable = path.join(cwd, executable);
            args = this.preargs.concat(this.extraargs || []);
            this.process = ChildProcess.spawn(this.application, args, { cwd: cwd, env: this.procEnv });
            this.process.stdout.on("data", this.stdout.bind(this));
            this.process.stderr.on("data", this.stderr.bind(this));
            this.process.on("exit", (() => { this.emit("quit"); }).bind(this));
            this.process.on("error", ((err) => { this.emit("launcherror", err); }).bind(this));
            const promises = this.initCommands(target, cwd, true);
            if (target.startsWith("extended-remote")) {
                promises.push(this.sendCommand("target-select " + target));
                if (executable)
                    promises.push(this.sendCommand("file-symbol-file \"" + escape(executable) + "\""));
            }
            else {
                // Attach to local process
                if (executable)
                    promises.push(this.sendCommand("file-exec-and-symbols \"" + escape(executable) + "\""));
                promises.push(this.sendCommand("target-attach " + target));
            }
            Promise.all(promises).then(() => {
                this.emit("debug-ready");
                resolve(undefined);
            }, reject);
        });
    }
    connect(cwd, executable, target) {
        return new Promise((resolve, reject) => {
            let args = [];
            if (executable && !path.isAbsolute(executable))
                executable = path.join(cwd, executable);
            args = this.preargs.concat(this.extraargs || []);
            if (executable)
                args = args.concat([executable]);
            this.process = ChildProcess.spawn(this.application, args, { cwd: cwd, env: this.procEnv });
            this.process.stdout.on("data", this.stdout.bind(this));
            this.process.stderr.on("data", this.stderr.bind(this));
            this.process.on("exit", (() => { this.emit("quit"); }).bind(this));
            this.process.on("error", ((err) => { this.emit("launcherror", err); }).bind(this));
            const promises = this.initCommands(target, cwd, true);
            promises.push(this.sendCommand("target-select remote " + target));
            Promise.all(promises).then(() => {
                this.emit("debug-ready");
                resolve(undefined);
            }, reject);
        });
    }
    stdout(data) {
        if (trace)
            this.log("stderr", "stdout: " + data);
        if (typeof data == "string")
            this.buffer += data;
        else
            this.buffer += data.toString("utf8");
        const end = this.buffer.lastIndexOf('\n');
        if (end != -1) {
            this.onOutput(this.buffer.substr(0, end));
            this.buffer = this.buffer.substr(end + 1);
        }
        if (this.buffer.length) {
            if (this.onOutputPartial(this.buffer)) {
                this.buffer = "";
            }
        }
    }
    stderr(data) {
        if (typeof data == "string")
            this.errbuf += data;
        else
            this.errbuf += data.toString("utf8");
        const end = this.errbuf.lastIndexOf('\n');
        if (end != -1) {
            this.onOutputStderr(this.errbuf.substr(0, end));
            this.errbuf = this.errbuf.substr(end + 1);
        }
        if (this.errbuf.length) {
            this.logNoNewLine("stderr", this.errbuf);
            this.errbuf = "";
        }
    }
    onOutputStderr(lines) {
        lines = lines.split('\n');
        lines.forEach(line => {
            this.log("stderr", line);
        });
    }
    onOutputPartial(line) {
        if (couldBeOutput(line)) {
            this.logNoNewLine("stdout", line);
            return true;
        }
        return false;
    }
    onOutput(lines) {
        lines = lines.split('\n');
        lines.forEach(line => {
            if (couldBeOutput(line)) {
                if (!gdbMatch.exec(line))
                    this.log("stdout", line);
            }
            else {
                const parsed = mi_parse_1.parseMI(line);
                if (this.debugOutput)
                    this.log("log", "GDB -> App: " + JSON.stringify(parsed));
                let handled = false;
                if (parsed.token !== undefined) {
                    if (this.handlers[parsed.token]) {
                        this.handlers[parsed.token](parsed);
                        delete this.handlers[parsed.token];
                        handled = true;
                    }
                }
                if (!handled && parsed.resultRecords && parsed.resultRecords.resultClass == "error") {
                    this.log("stderr", parsed.result("msg") || line);
                }
                if (parsed.outOfBandRecord) {
                    parsed.outOfBandRecord.forEach(record => {
                        if (record.isStream) {
                            this.log(record.type, record.content);
                        }
                        else {
                            if (record.type == "exec") {
                                this.emit("exec-async-output", parsed);
                                if (record.asyncClass == "running")
                                    this.emit("running", parsed);
                                else if (record.asyncClass == "stopped") {
                                    const reason = parsed.record("reason");
                                    if (reason === undefined) {
                                        if (trace)
                                            this.log("stderr", "stop (no reason given)");
                                        // attaching to a process stops, but does not provide a reason
                                        // also python generated interrupt seems to only produce this
                                        this.emit("step-other", parsed);
                                    }
                                    else {
                                        if (trace)
                                            this.log("stderr", "stop: " + reason);
                                        switch (reason) {
                                            case "breakpoint-hit":
                                                this.emit("breakpoint", parsed);
                                                break;
                                            case "watchpoint-trigger":
                                            case "read-watchpoint-trigger":
                                            case "access-watchpoint-trigger":
                                                this.emit("watchpoint", parsed);
                                                break;
                                            case "function-finished":
                                            // identical result -> send step-end
                                            // this.emit("step-out-end", parsed);
                                            // break;
                                            case "location-reached":
                                            case "end-stepping-range":
                                                this.emit("step-end", parsed);
                                                break;
                                            case "watchpoint-scope":
                                            case "solib-event":
                                            case "syscall-entry":
                                            case "syscall-return":
                                                // TODO: inform the user
                                                this.emit("step-end", parsed);
                                                break;
                                            case "fork":
                                            case "vfork":
                                            case "exec":
                                                // TODO: inform the user, possibly add second inferiour
                                                this.emit("step-end", parsed);
                                                break;
                                            case "signal-received":
                                                this.emit("signal-stop", parsed);
                                                break;
                                            case "exited-normally":
                                                this.emit("exited-normally", parsed);
                                                break;
                                            case "exited": // exit with error code != 0
                                                this.log("stderr", "Program exited with code " + parsed.record("exit-code"));
                                                this.emit("exited-normally", parsed);
                                                break;
                                            // case "exited-signalled":	// consider handling that explicit possible
                                            // 	this.log("stderr", "Program exited because of signal " + parsed.record("signal"));
                                            // 	this.emit("stoped", parsed);
                                            // 	break;
                                            default:
                                                this.log("console", "Not implemented stop reason (assuming exception): " + reason);
                                                this.emit("stopped", parsed);
                                                break;
                                        }
                                    }
                                }
                                else
                                    this.log("log", JSON.stringify(parsed));
                            }
                            else if (record.type == "notify") {
                                if (record.asyncClass == "thread-created") {
                                    this.emit("thread-created", parsed);
                                }
                                else if (record.asyncClass == "thread-exited") {
                                    this.emit("thread-exited", parsed);
                                }
                            }
                        }
                    });
                    handled = true;
                }
                if (parsed.token == undefined && parsed.resultRecords == undefined && parsed.outOfBandRecord.length == 0)
                    handled = true;
                if (!handled)
                    this.log("log", "Unhandled: " + JSON.stringify(parsed));
            }
        });
    }
    start(runToStart) {
        const options = [];
        if (runToStart)
            options.push("--start");
        const startCommand = ["exec-run"].concat(options).join(" ");
        return new Promise((resolve, reject) => {
            this.log("console", "Running executable");
            this.sendCommand(startCommand).then((info) => {
                if (info.resultRecords.resultClass == "running")
                    resolve(undefined);
                else
                    reject();
            }, reject);
        });
    }
    stop() {
        if (this.isSSH) {
            const proc = this.stream;
            const to = setTimeout(() => {
                proc.signal("KILL");
            }, 1000);
            this.stream.on("exit", function (code) {
                clearTimeout(to);
            });
            this.sendRaw("-gdb-exit");
        }
        else {
            const proc = this.process;
            const to = setTimeout(() => {
                process.kill(-proc.pid);
            }, 1000);
            this.process.on("exit", function (code) {
                clearTimeout(to);
            });
            this.sendRaw("-gdb-exit");
        }
    }
    detach() {
        const proc = this.process;
        const to = setTimeout(() => {
            process.kill(-proc.pid);
        }, 1000);
        this.process.on("exit", function (code) {
            clearTimeout(to);
        });
        this.sendRaw("-target-detach");
    }
    interrupt() {
        if (trace)
            this.log("stderr", "interrupt");
        return new Promise((resolve, reject) => {
            this.sendCommand("exec-interrupt").then((info) => {
                resolve(info.resultRecords.resultClass == "done");
            }, reject);
        });
    }
    continue(reverse = false) {
        if (trace)
            this.log("stderr", "continue");
        return new Promise((resolve, reject) => {
            this.sendCommand("exec-continue" + (reverse ? " --reverse" : "")).then((info) => {
                resolve(info.resultRecords.resultClass == "running");
            }, reject);
        });
    }
    next(reverse = false) {
        if (trace)
            this.log("stderr", "next");
        return new Promise((resolve, reject) => {
            this.sendCommand("exec-next" + (reverse ? " --reverse" : "")).then((info) => {
                resolve(info.resultRecords.resultClass == "running");
            }, reject);
        });
    }
    step(reverse = false) {
        if (trace)
            this.log("stderr", "step");
        return new Promise((resolve, reject) => {
            this.sendCommand("exec-step" + (reverse ? " --reverse" : "")).then((info) => {
                resolve(info.resultRecords.resultClass == "running");
            }, reject);
        });
    }
    stepOut(reverse = false) {
        if (trace)
            this.log("stderr", "stepOut");
        return new Promise((resolve, reject) => {
            this.sendCommand("exec-finish" + (reverse ? " --reverse" : "")).then((info) => {
                resolve(info.resultRecords.resultClass == "running");
            }, reject);
        });
    }
    goto(filename, line) {
        if (trace)
            this.log("stderr", "goto");
        return new Promise((resolve, reject) => {
            const target = '"' + (filename ? escape(filename) + ":" : "") + line + '"';
            this.sendCommand("break-insert -t " + target).then(() => {
                this.sendCommand("exec-jump " + target).then((info) => {
                    resolve(info.resultRecords.resultClass == "running");
                }, reject);
            }, reject);
        });
    }
    changeVariable(name, rawValue) {
        if (trace)
            this.log("stderr", "changeVariable");
        return this.sendCommand("gdb-set var " + name + "=" + rawValue);
    }
    loadBreakPoints(breakpoints) {
        if (trace)
            this.log("stderr", "loadBreakPoints");
        const promisses = [];
        breakpoints.forEach(breakpoint => {
            promisses.push(this.addBreakPoint(breakpoint));
        });
        return Promise.all(promisses);
    }
    setBreakPointCondition(bkptNum, condition) {
        if (trace)
            this.log("stderr", "setBreakPointCondition");
        return this.sendCommand("break-condition " + bkptNum + " " + condition);
    }
    setEntryBreakPoint(entryPoint) {
        return this.sendCommand("break-insert -t -f " + entryPoint);
    }
    addBreakPoint(breakpoint) {
        if (trace)
            this.log("stderr", "addBreakPoint");
        return new Promise((resolve, reject) => {
            if (this.breakpoints.has(breakpoint))
                return resolve([false, undefined]);
            let location = "";
            if (breakpoint.countCondition) {
                if (breakpoint.countCondition[0] == ">")
                    location += "-i " + numRegex.exec(breakpoint.countCondition.substr(1))[0] + " ";
                else {
                    const match = numRegex.exec(breakpoint.countCondition)[0];
                    if (match.length != breakpoint.countCondition.length) {
                        this.log("stderr", "Unsupported break count expression: '" + breakpoint.countCondition + "'. Only supports 'X' for breaking once after X times or '>X' for ignoring the first X breaks");
                        location += "-t ";
                    }
                    else if (parseInt(match) != 0)
                        location += "-t -i " + parseInt(match) + " ";
                }
            }
            if (breakpoint.raw)
                location += '"' + escape(breakpoint.raw) + '"';
            else
                location += '"' + escape(breakpoint.file) + ":" + breakpoint.line + '"';
            this.sendCommand("break-insert -f " + location).then((result) => {
                if (result.resultRecords.resultClass == "done") {
                    const bkptNum = parseInt(result.result("bkpt.number"));
                    const newBrk = {
                        file: breakpoint.file ? breakpoint.file : result.result("bkpt.file"),
                        raw: breakpoint.raw,
                        line: parseInt(result.result("bkpt.line")),
                        condition: breakpoint.condition
                    };
                    if (breakpoint.condition) {
                        this.setBreakPointCondition(bkptNum, breakpoint.condition).then((result) => {
                            if (result.resultRecords.resultClass == "done") {
                                this.breakpoints.set(newBrk, bkptNum);
                                resolve([true, newBrk]);
                            }
                            else {
                                resolve([false, undefined]);
                            }
                        }, reject);
                    }
                    else {
                        this.breakpoints.set(newBrk, bkptNum);
                        resolve([true, newBrk]);
                    }
                }
                else {
                    reject(result);
                }
            }, reject);
        });
    }
    removeBreakPoint(breakpoint) {
        if (trace)
            this.log("stderr", "removeBreakPoint");
        return new Promise((resolve, reject) => {
            if (!this.breakpoints.has(breakpoint))
                return resolve(false);
            this.sendCommand("break-delete " + this.breakpoints.get(breakpoint)).then((result) => {
                if (result.resultRecords.resultClass == "done") {
                    this.breakpoints.delete(breakpoint);
                    resolve(true);
                }
                else
                    resolve(false);
            });
        });
    }
    clearBreakPoints(source) {
        if (trace)
            this.log("stderr", "clearBreakPoints");
        return new Promise((resolve, reject) => {
            const promises = [];
            const breakpoints = this.breakpoints;
            this.breakpoints = new Map();
            breakpoints.forEach((k, index) => {
                if (index.file === source) {
                    promises.push(this.sendCommand("break-delete " + k).then((result) => {
                        if (result.resultRecords.resultClass == "done")
                            resolve(true);
                        else
                            resolve(false);
                    }));
                }
                else {
                    this.breakpoints.set(index, k);
                }
            });
            Promise.all(promises).then(resolve, reject);
        });
    }
    getThreads() {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "getThreads");
            const command = "thread-info";
            const result = yield this.sendCommand(command);
            const threads = result.result("threads");
            const ret = [];
            return threads.map(element => {
                const ret = {
                    id: parseInt(mi_parse_1.MINode.valueOf(element, "id")),
                    targetId: mi_parse_1.MINode.valueOf(element, "target-id")
                };
                ret.name = mi_parse_1.MINode.valueOf(element, "details")
                    || undefined;
                return ret;
            });
        });
    }
    getStack(startFrame, maxLevels, thread) {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "getStack");
            const options = [];
            if (thread != 0)
                options.push("--thread " + thread);
            const depth = (yield this.sendCommand(["stack-info-depth"].concat(options).join(" "))).result("depth").valueOf();
            const lowFrame = startFrame ? startFrame : 0;
            const highFrame = (maxLevels ? Math.min(depth, lowFrame + maxLevels) : depth) - 1;
            if (highFrame < lowFrame)
                return [];
            options.push(lowFrame.toString());
            options.push(highFrame.toString());
            const result = yield this.sendCommand(["stack-list-frames"].concat(options).join(" "));
            const stack = result.result("stack");
            return stack.map(element => {
                const level = mi_parse_1.MINode.valueOf(element, "@frame.level");
                const addr = mi_parse_1.MINode.valueOf(element, "@frame.addr");
                const func = mi_parse_1.MINode.valueOf(element, "@frame.func");
                const filename = mi_parse_1.MINode.valueOf(element, "@frame.file");
                let file = mi_parse_1.MINode.valueOf(element, "@frame.fullname");
                if (file) {
                    if (this.isSSH)
                        file = path.posix.normalize(file);
                    else
                        file = path.normalize(file);
                }
                let line = 0;
                const lnstr = mi_parse_1.MINode.valueOf(element, "@frame.line");
                if (lnstr)
                    line = parseInt(lnstr);
                const from = parseInt(mi_parse_1.MINode.valueOf(element, "@frame.from"));
                return {
                    address: addr,
                    fileName: filename,
                    file: file,
                    function: func || from,
                    level: level,
                    line: line
                };
            });
        });
    }
    getStackVariables(thread, frame) {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "getStackVariables");
            const result = yield this.sendCommand(`stack-list-variables --thread ${thread} --frame ${frame} --simple-values`);
            const variables = result.result("variables");
            const ret = [];
            for (const element of variables) {
                const key = mi_parse_1.MINode.valueOf(element, "name");
                const value = mi_parse_1.MINode.valueOf(element, "value");
                const type = mi_parse_1.MINode.valueOf(element, "type");
                ret.push({
                    name: key,
                    valueStr: value,
                    type: type,
                    raw: element
                });
            }
            return ret;
        });
    }
    examineMemory(from, length) {
        if (trace)
            this.log("stderr", "examineMemory");
        return new Promise((resolve, reject) => {
            this.sendCommand("data-read-memory-bytes 0x" + from.toString(16) + " " + length).then((result) => {
                resolve(result.result("memory[0].contents"));
            }, reject);
        });
    }
    evalExpression(name, thread, frame) {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "evalExpression");
            let command = "data-evaluate-expression ";
            if (thread != 0) {
                command += `--thread ${thread} --frame ${frame} `;
            }
            command += name;
            return yield this.sendCommand(command);
        });
    }
    varCreate(expression, name = "-", frame = "@") {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "varCreate");
            const res = yield this.sendCommand(`var-create ${this.quote(name)} ${frame} "${expression}"`);
            return new backend_1.VariableObject(res.result(""));
        });
    }
    varEvalExpression(name) {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "varEvalExpression");
            return this.sendCommand(`var-evaluate-expression ${this.quote(name)}`);
        });
    }
    varListChildren(name) {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "varListChildren");
            //TODO: add `from` and `to` arguments
            const res = yield this.sendCommand(`var-list-children --all-values ${this.quote(name)}`);
            const children = res.result("children") || [];
            const omg = children.map(child => new backend_1.VariableObject(child[1]));
            return omg;
        });
    }
    varUpdate(name = "*") {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "varUpdate");
            return this.sendCommand(`var-update --all-values ${this.quote(name)}`);
        });
    }
    varAssign(name, rawValue) {
        return __awaiter(this, void 0, void 0, function* () {
            if (trace)
                this.log("stderr", "varAssign");
            return this.sendCommand(`var-assign ${this.quote(name)} ${rawValue}`);
        });
    }
    logNoNewLine(type, msg) {
        this.emit("msg", type, msg);
    }
    log(type, msg) {
        this.emit("msg", type, msg[msg.length - 1] == '\n' ? msg : (msg + "\n"));
    }
    sendUserInput(command, threadId = 0, frameLevel = 0) {
        if (command.startsWith("-")) {
            return this.sendCommand(command.substr(1));
        }
        else {
            return this.sendCliCommand(command, threadId, frameLevel);
        }
    }
    sendRaw(raw) {
        if (this.printCalls)
            this.log("log", raw);
        if (this.isSSH)
            this.stream.write(raw + "\n");
        else
            this.process.stdin.write(raw + "\n");
    }
    sendCliCommand(command, threadId = 0, frameLevel = 0) {
        let miCommand = "interpreter-exec ";
        if (threadId != 0) {
            miCommand += `--thread ${threadId} --frame ${frameLevel} `;
        }
        miCommand += `console "${command.replace(/[\\"']/g, '\\$&')}"`;
        return this.sendCommand(miCommand);
    }
    sendCommand(command, suppressFailure = false) {
        const sel = this.currentToken++;
        return new Promise((resolve, reject) => {
            this.handlers[sel] = (node) => {
                if (node && node.resultRecords && node.resultRecords.resultClass === "error") {
                    if (suppressFailure) {
                        this.log("stderr", `WARNING: Error executing command '${command}'`);
                        resolve(node);
                    }
                    else
                        reject(new backend_1.MIError(node.result("msg") || "Internal error", command));
                }
                else
                    resolve(node);
            };
            this.sendRaw(sel + "-" + command);
        });
    }
    isReady() {
        return this.isSSH ? this.sshReady : !!this.process;
    }
    quote(text) {
        // only escape if text contains non-word or non-path characters such as whitespace or quotes
        return /^-|[^\w\d\/_\-\.]/g.test(text) ? ('"' + escape(text) + '"') : text;
    }
}
exports.MI2 = MI2;
//# sourceMappingURL=mi2.js.map