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
exports.MI2DebugSession = exports.RunCommand = void 0;
const DebugAdapter = require("vscode-debugadapter");
const vscode_debugadapter_1 = require("vscode-debugadapter");
const backend_1 = require("./backend/backend");
const mi_parse_1 = require("./backend/mi_parse");
const gdb_expansion_1 = require("./backend/gdb_expansion");
const path_1 = require("path");
const systemPath = require("path");
const net = require("net");
const os = require("os");
const fs = require("fs");
class ExtendedVariable {
    constructor(name, options) {
        this.name = name;
        this.options = options;
    }
}
var RunCommand;
(function (RunCommand) {
    RunCommand[RunCommand["CONTINUE"] = 0] = "CONTINUE";
    RunCommand[RunCommand["RUN"] = 1] = "RUN";
    RunCommand[RunCommand["NONE"] = 2] = "NONE";
})(RunCommand = exports.RunCommand || (exports.RunCommand = {}));
const STACK_HANDLES_START = 1000;
const VAR_HANDLES_START = 512 * 256 + 1000;
class MI2DebugSession extends vscode_debugadapter_1.DebugSession {
    constructor(debuggerLinesStartAt1, isServer = false) {
        super(debuggerLinesStartAt1, isServer);
        this.variableHandles = new vscode_debugadapter_1.Handles(VAR_HANDLES_START);
        this.variableHandlesReverse = {};
    }
    initDebugger() {
        this.miDebugger.on("launcherror", this.launchError.bind(this));
        this.miDebugger.on("quit", this.quitEvent.bind(this));
        this.miDebugger.on("exited-normally", this.quitEvent.bind(this));
        this.miDebugger.on("stopped", this.stopEvent.bind(this));
        this.miDebugger.on("msg", this.handleMsg.bind(this));
        this.miDebugger.on("breakpoint", this.handleBreakpoint.bind(this));
        this.miDebugger.on("watchpoint", this.handleBreak.bind(this)); // consider to parse old/new, too (otherwise it is in the console only)
        this.miDebugger.on("step-end", this.handleBreak.bind(this));
        // this.miDebugger.on("step-out-end", this.handleBreak.bind(this));  // was combined into step-end
        this.miDebugger.on("step-other", this.handleBreak.bind(this));
        this.miDebugger.on("signal-stop", this.handlePause.bind(this));
        this.miDebugger.on("thread-created", this.threadCreatedEvent.bind(this));
        this.miDebugger.on("thread-exited", this.threadExitedEvent.bind(this));
        this.miDebugger.once("debug-ready", (() => this.sendEvent(new vscode_debugadapter_1.InitializedEvent())));
        try {
            this.commandServer = net.createServer(c => {
                c.on("data", data => {
                    const rawCmd = data.toString();
                    const spaceIndex = rawCmd.indexOf(" ");
                    let func = rawCmd;
                    let args = [];
                    if (spaceIndex != -1) {
                        func = rawCmd.substr(0, spaceIndex);
                        args = JSON.parse(rawCmd.substr(spaceIndex + 1));
                    }
                    Promise.resolve(this.miDebugger[func].apply(this.miDebugger, args)).then(data => {
                        c.write(data.toString());
                    });
                });
            });
            this.commandServer.on("error", err => {
                if (process.platform != "win32")
                    this.handleMsg("stderr", "Code-Debug WARNING: Utility Command Server: Error in command socket " + err.toString() + "\nCode-Debug WARNING: The examine memory location command won't work");
            });
            if (!fs.existsSync(systemPath.join(os.tmpdir(), "code-debug-sockets")))
                fs.mkdirSync(systemPath.join(os.tmpdir(), "code-debug-sockets"));
            this.commandServer.listen(this.serverPath = systemPath.join(os.tmpdir(), "code-debug-sockets", ("Debug-Instance-" + Math.floor(Math.random() * 36 * 36 * 36 * 36).toString(36)).toLowerCase()));
        }
        catch (e) {
            if (process.platform != "win32")
                this.handleMsg("stderr", "Code-Debug WARNING: Utility Command Server: Failed to start " + e.toString() + "\nCode-Debug WARNING: The examine memory location command won't work");
        }
    }
    setValuesFormattingMode(mode) {
        switch (mode) {
            case "disabled":
                this.useVarObjects = true;
                this.miDebugger.prettyPrint = false;
                break;
            case "prettyPrinters":
                this.useVarObjects = true;
                this.miDebugger.prettyPrint = true;
                break;
            case "parseText":
            default:
                this.useVarObjects = false;
                this.miDebugger.prettyPrint = false;
        }
    }
    handleMsg(type, msg) {
        if (type == "target")
            type = "stdout";
        if (type == "log")
            type = "stderr";
        this.sendEvent(new vscode_debugadapter_1.OutputEvent(msg, type));
    }
    handleBreakpoint(info) {
        const event = new vscode_debugadapter_1.StoppedEvent("breakpoint", parseInt(info.record("thread-id")));
        event.body.allThreadsStopped = info.record("stopped-threads") == "all";
        this.sendEvent(event);
    }
    handleBreak(info) {
        const event = new vscode_debugadapter_1.StoppedEvent("step", info ? parseInt(info.record("thread-id")) : 1);
        event.body.allThreadsStopped = info ? info.record("stopped-threads") == "all" : true;
        this.sendEvent(event);
    }
    handlePause(info) {
        const event = new vscode_debugadapter_1.StoppedEvent("user request", parseInt(info.record("thread-id")));
        event.body.allThreadsStopped = info.record("stopped-threads") == "all";
        this.sendEvent(event);
    }
    stopEvent(info) {
        if (!this.started)
            this.crashed = true;
        if (!this.quit) {
            const event = new vscode_debugadapter_1.StoppedEvent("exception", parseInt(info.record("thread-id")));
            event.body.allThreadsStopped = info.record("stopped-threads") == "all";
            this.sendEvent(event);
        }
    }
    threadCreatedEvent(info) {
        this.sendEvent(new vscode_debugadapter_1.ThreadEvent("started", info.record("id")));
    }
    threadExitedEvent(info) {
        this.sendEvent(new vscode_debugadapter_1.ThreadEvent("exited", info.record("id")));
    }
    quitEvent() {
        this.quit = true;
        this.sendEvent(new vscode_debugadapter_1.TerminatedEvent());
        if (this.serverPath)
            fs.unlink(this.serverPath, (err) => {
                console.error("Failed to unlink debug server");
            });
    }
    launchError(err) {
        this.handleMsg("stderr", "Could not start debugger process, does the program exist in filesystem?\n");
        this.handleMsg("stderr", err.toString() + "\n");
        this.quitEvent();
    }
    disconnectRequest(response, args) {
        if (this.attached)
            this.miDebugger.detach();
        else
            this.miDebugger.stop();
        this.commandServer.close();
        this.commandServer = undefined;
        this.sendResponse(response);
    }
    setVariableRequest(response, args) {
        return __awaiter(this, void 0, void 0, function* () {
            try {
                if (this.useVarObjects) {
                    let name = args.name;
                    if (args.variablesReference >= VAR_HANDLES_START) {
                        const parent = this.variableHandles.get(args.variablesReference);
                        name = `${parent.name}.${name}`;
                    }
                    const res = yield this.miDebugger.varAssign(name, args.value);
                    response.body = {
                        value: res.result("value")
                    };
                }
                else {
                    yield this.miDebugger.changeVariable(args.name, args.value);
                    response.body = {
                        value: args.value
                    };
                }
                this.sendResponse(response);
            }
            catch (err) {
                this.sendErrorResponse(response, 11, `Could not continue: ${err}`);
            }
        });
    }
    setFunctionBreakPointsRequest(response, args) {
        const all = [];
        args.breakpoints.forEach(brk => {
            all.push(this.miDebugger.addBreakPoint({ raw: brk.name, condition: brk.condition, countCondition: brk.hitCondition }));
        });
        Promise.all(all).then(brkpoints => {
            const finalBrks = [];
            brkpoints.forEach(brkp => {
                if (brkp[0])
                    finalBrks.push({ line: brkp[1].line });
            });
            response.body = {
                breakpoints: finalBrks
            };
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 10, msg.toString());
        });
    }
    setBreakPointsRequest(response, args) {
        this.miDebugger.clearBreakPoints(args.source.path).then(() => {
            let path = args.source.path;
            if (this.isSSH) {
                // convert local path to ssh path
                if (path.indexOf("\\") != -1)
                    path = path.replace(/\\/g, "/").toLowerCase();
                // ideCWD is the local path, gdbCWD is the ssh path, both had the replacing \ -> / done up-front
                for (let [ideCWD, gdbCWD] of this.sourceFileMap) {
                    if (path.startsWith(ideCWD)) {
                        path = path_1.posix.relative(ideCWD, path);
                        path = path_1.posix.join(gdbCWD, path); // we combined a guaranteed path with relative one (works with GDB both on GNU/Linux and Win32)
                        break;
                    }
                }
            }
            const all = args.breakpoints.map(brk => {
                return this.miDebugger.addBreakPoint({ file: path, line: brk.line, condition: brk.condition, countCondition: brk.hitCondition });
            });
            Promise.all(all).then(brkpoints => {
                const finalBrks = [];
                brkpoints.forEach(brkp => {
                    // TODO: Currently all breakpoints returned are marked as verified,
                    // which leads to verified breakpoints on a broken lldb.
                    if (brkp[0])
                        finalBrks.push(new DebugAdapter.Breakpoint(true, brkp[1].line));
                });
                response.body = {
                    breakpoints: finalBrks
                };
                this.sendResponse(response);
            }, msg => {
                this.sendErrorResponse(response, 9, msg.toString());
            });
        }, msg => {
            this.sendErrorResponse(response, 9, msg.toString());
        });
    }
    threadsRequest(response) {
        if (!this.miDebugger) {
            this.sendResponse(response);
            return;
        }
        this.miDebugger.getThreads().then(threads => {
            response.body = {
                threads: []
            };
            for (const thread of threads) {
                let threadName = thread.name || thread.targetId || "<unnamed>";
                response.body.threads.push(new vscode_debugadapter_1.Thread(thread.id, thread.id + ":" + threadName));
            }
            this.sendResponse(response);
        }).catch(error => {
            this.sendErrorResponse(response, 17, `Could not get threads: ${error}`);
        });
    }
    // Supports 65535 threads.
    threadAndLevelToFrameId(threadId, level) {
        return level << 16 | threadId;
    }
    frameIdToThreadAndLevel(frameId) {
        return [frameId & 0xffff, frameId >> 16];
    }
    stackTraceRequest(response, args) {
        this.miDebugger.getStack(args.startFrame, args.levels, args.threadId).then(stack => {
            const ret = [];
            stack.forEach(element => {
                let source = undefined;
                let path = element.file;
                if (path) {
                    if (this.isSSH) {
                        // convert ssh path to local path
                        if (path.indexOf("\\") != -1)
                            path = path.replace(/\\/g, "/").toLowerCase();
                        // ideCWD is the local path, gdbCWD is the ssh path, both had the replacing \ -> / done up-front
                        for (let [ideCWD, gdbCWD] of this.sourceFileMap) {
                            if (path.startsWith(gdbCWD)) {
                                path = path_1.posix.relative(gdbCWD, path); // only operates on "/" paths
                                path = systemPath.resolve(ideCWD, path); // will do the conversion to "\" on Win32
                                break;
                            }
                        }
                    }
                    else if (process.platform === "win32") {
                        if (path.startsWith("\\cygdrive\\") || path.startsWith("/cygdrive/")) {
                            path = path[10] + ":" + path.substr(11); // replaces /cygdrive/c/foo/bar.txt with c:/foo/bar.txt
                        }
                    }
                    source = new vscode_debugadapter_1.Source(element.fileName, path);
                }
                ret.push(new vscode_debugadapter_1.StackFrame(this.threadAndLevelToFrameId(args.threadId, element.level), element.function + "@" + element.address, source, element.line, 0));
            });
            response.body = {
                stackFrames: ret
            };
            this.sendResponse(response);
        }, err => {
            this.sendErrorResponse(response, 12, `Failed to get Stack Trace: ${err.toString()}`);
        });
    }
    configurationDoneRequest(response, args) {
        const promises = [];
        let entryPoint = undefined;
        let runToStart = false;
        // Setup temporary breakpoint for the entry point if needed.
        switch (this.initialRunCommand) {
            case RunCommand.CONTINUE:
            case RunCommand.NONE:
                if (typeof this.stopAtEntry == 'boolean' && this.stopAtEntry)
                    entryPoint = "main"; // sensible default
                else if (typeof this.stopAtEntry == 'string')
                    entryPoint = this.stopAtEntry;
                break;
            case RunCommand.RUN:
                if (typeof this.stopAtEntry == 'boolean' && this.stopAtEntry) {
                    if (this.miDebugger.features.includes("exec-run-start-option"))
                        runToStart = true;
                    else
                        entryPoint = "main"; // sensible fallback
                }
                else if (typeof this.stopAtEntry == 'string')
                    entryPoint = this.stopAtEntry;
                break;
            default:
                throw new Error('Unhandled run command: ' + RunCommand[this.initialRunCommand]);
        }
        if (entryPoint)
            promises.push(this.miDebugger.setEntryBreakPoint(entryPoint));
        switch (this.initialRunCommand) {
            case RunCommand.CONTINUE:
                promises.push(this.miDebugger.continue().then(() => {
                    // Some debuggers will provide an out-of-band status that they are stopped
                    // when attaching (e.g., gdb), so the client assumes we are stopped and gets
                    // confused if we start running again on our own.
                    //
                    // If we don't send this event, the client may start requesting data (such as
                    // stack frames, local variables, etc.) since they believe the target is
                    // stopped.  Furthermore the client may not be indicating the proper status
                    // to the user (may indicate stopped when the target is actually running).
                    this.sendEvent(new vscode_debugadapter_1.ContinuedEvent(1, true));
                }));
                break;
            case RunCommand.RUN:
                promises.push(this.miDebugger.start(runToStart).then(() => {
                    this.started = true;
                    if (this.crashed)
                        this.handlePause(undefined);
                }));
                break;
            case RunCommand.NONE:
                // Not all debuggers seem to provide an out-of-band status that they are stopped
                // when attaching (e.g., lldb), so the client assumes we are running and gets
                // confused when we don't actually run or continue.  Therefore, we'll force a
                // stopped event to be sent to the client (just in case) to synchronize the state.
                const event = new vscode_debugadapter_1.StoppedEvent("pause", 1);
                event.body.description = "paused on attach";
                event.body.allThreadsStopped = true;
                this.sendEvent(event);
                break;
            default:
                throw new Error('Unhandled run command: ' + RunCommand[this.initialRunCommand]);
        }
        Promise.all(promises).then(() => {
            this.sendResponse(response);
        }).catch(err => {
            this.sendErrorResponse(response, 18, `Could not run/continue: ${err.toString()}`);
        });
    }
    scopesRequest(response, args) {
        const scopes = new Array();
        scopes.push(new vscode_debugadapter_1.Scope("Local", STACK_HANDLES_START + (parseInt(args.frameId) || 0), false));
        response.body = {
            scopes: scopes
        };
        this.sendResponse(response);
    }
    variablesRequest(response, args) {
        return __awaiter(this, void 0, void 0, function* () {
            const variables = [];
            let id;
            if (args.variablesReference < VAR_HANDLES_START) {
                id = args.variablesReference - STACK_HANDLES_START;
            }
            else {
                id = this.variableHandles.get(args.variablesReference);
            }
            const createVariable = (arg, options) => {
                if (options)
                    return this.variableHandles.create(new ExtendedVariable(arg, options));
                else
                    return this.variableHandles.create(arg);
            };
            const findOrCreateVariable = (varObj) => {
                let id;
                if (this.variableHandlesReverse.hasOwnProperty(varObj.name)) {
                    id = this.variableHandlesReverse[varObj.name];
                }
                else {
                    id = createVariable(varObj);
                    this.variableHandlesReverse[varObj.name] = id;
                }
                return varObj.isCompound() ? id : 0;
            };
            if (typeof id == "number") {
                let stack;
                try {
                    const [threadId, level] = this.frameIdToThreadAndLevel(id);
                    stack = yield this.miDebugger.getStackVariables(threadId, level);
                    for (const variable of stack) {
                        if (this.useVarObjects) {
                            try {
                                const varObjName = `var_${id}_${variable.name}`;
                                let varObj;
                                try {
                                    const changes = yield this.miDebugger.varUpdate(varObjName);
                                    const changelist = changes.result("changelist");
                                    changelist.forEach((change) => {
                                        const name = mi_parse_1.MINode.valueOf(change, "name");
                                        const vId = this.variableHandlesReverse[name];
                                        const v = this.variableHandles.get(vId);
                                        v.applyChanges(change);
                                    });
                                    const varId = this.variableHandlesReverse[varObjName];
                                    varObj = this.variableHandles.get(varId);
                                }
                                catch (err) {
                                    if (err instanceof backend_1.MIError && err.message == "Variable object not found") {
                                        varObj = yield this.miDebugger.varCreate(variable.name, varObjName);
                                        const varId = findOrCreateVariable(varObj);
                                        varObj.exp = variable.name;
                                        varObj.id = varId;
                                    }
                                    else {
                                        throw err;
                                    }
                                }
                                variables.push(varObj.toProtocolVariable());
                            }
                            catch (err) {
                                variables.push({
                                    name: variable.name,
                                    value: `<${err}>`,
                                    variablesReference: 0
                                });
                            }
                        }
                        else {
                            if (variable.valueStr !== undefined) {
                                let expanded = gdb_expansion_1.expandValue(createVariable, `{${variable.name}=${variable.valueStr})`, "", variable.raw);
                                if (expanded) {
                                    if (typeof expanded[0] == "string")
                                        expanded = [
                                            {
                                                name: "<value>",
                                                value: prettyStringArray(expanded),
                                                variablesReference: 0
                                            }
                                        ];
                                    variables.push(expanded[0]);
                                }
                            }
                            else
                                variables.push({
                                    name: variable.name,
                                    type: variable.type,
                                    value: "<unknown>",
                                    variablesReference: createVariable(variable.name)
                                });
                        }
                    }
                    response.body = {
                        variables: variables
                    };
                    this.sendResponse(response);
                }
                catch (err) {
                    this.sendErrorResponse(response, 1, `Could not expand variable: ${err}`);
                }
            }
            else if (typeof id == "string") {
                // Variable members
                let variable;
                try {
                    // TODO: this evals on an (effectively) unknown thread for multithreaded programs.
                    variable = yield this.miDebugger.evalExpression(JSON.stringify(id), 0, 0);
                    try {
                        let expanded = gdb_expansion_1.expandValue(createVariable, variable.result("value"), id, variable);
                        if (!expanded) {
                            this.sendErrorResponse(response, 2, `Could not expand variable`);
                        }
                        else {
                            if (typeof expanded[0] == "string")
                                expanded = [
                                    {
                                        name: "<value>",
                                        value: prettyStringArray(expanded),
                                        variablesReference: 0
                                    }
                                ];
                            response.body = {
                                variables: expanded
                            };
                            this.sendResponse(response);
                        }
                    }
                    catch (e) {
                        this.sendErrorResponse(response, 2, `Could not expand variable: ${e}`);
                    }
                }
                catch (err) {
                    this.sendErrorResponse(response, 1, `Could not expand variable: ${err}`);
                }
            }
            else if (typeof id == "object") {
                if (id instanceof backend_1.VariableObject) {
                    // Variable members
                    let children;
                    try {
                        children = yield this.miDebugger.varListChildren(id.name);
                        const vars = children.map(child => {
                            const varId = findOrCreateVariable(child);
                            child.id = varId;
                            return child.toProtocolVariable();
                        });
                        response.body = {
                            variables: vars
                        };
                        this.sendResponse(response);
                    }
                    catch (err) {
                        this.sendErrorResponse(response, 1, `Could not expand variable: ${err}`);
                    }
                }
                else if (id instanceof ExtendedVariable) {
                    const varReq = id;
                    if (varReq.options.arg) {
                        const strArr = [];
                        let argsPart = true;
                        let arrIndex = 0;
                        const submit = () => {
                            response.body = {
                                variables: strArr
                            };
                            this.sendResponse(response);
                        };
                        const addOne = () => __awaiter(this, void 0, void 0, function* () {
                            // TODO: this evals on an (effectively) unknown thread for multithreaded programs.
                            const variable = yield this.miDebugger.evalExpression(JSON.stringify(`${varReq.name}+${arrIndex})`), 0, 0);
                            try {
                                const expanded = gdb_expansion_1.expandValue(createVariable, variable.result("value"), varReq.name, variable);
                                if (!expanded) {
                                    this.sendErrorResponse(response, 15, `Could not expand variable`);
                                }
                                else {
                                    if (typeof expanded == "string") {
                                        if (expanded == "<nullptr>") {
                                            if (argsPart)
                                                argsPart = false;
                                            else
                                                return submit();
                                        }
                                        else if (expanded[0] != '"') {
                                            strArr.push({
                                                name: "[err]",
                                                value: expanded,
                                                variablesReference: 0
                                            });
                                            return submit();
                                        }
                                        strArr.push({
                                            name: `[${(arrIndex++)}]`,
                                            value: expanded,
                                            variablesReference: 0
                                        });
                                        addOne();
                                    }
                                    else {
                                        strArr.push({
                                            name: "[err]",
                                            value: expanded,
                                            variablesReference: 0
                                        });
                                        submit();
                                    }
                                }
                            }
                            catch (e) {
                                this.sendErrorResponse(response, 14, `Could not expand variable: ${e}`);
                            }
                        });
                        addOne();
                    }
                    else
                        this.sendErrorResponse(response, 13, `Unimplemented variable request options: ${JSON.stringify(varReq.options)}`);
                }
                else {
                    response.body = {
                        variables: id
                    };
                    this.sendResponse(response);
                }
            }
            else {
                response.body = {
                    variables: variables
                };
                this.sendResponse(response);
            }
        });
    }
    pauseRequest(response, args) {
        this.miDebugger.interrupt().then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 3, `Could not pause: ${msg}`);
        });
    }
    reverseContinueRequest(response, args) {
        this.miDebugger.continue(true).then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 2, `Could not continue: ${msg}`);
        });
    }
    continueRequest(response, args) {
        this.miDebugger.continue().then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 2, `Could not continue: ${msg}`);
        });
    }
    stepBackRequest(response, args) {
        this.miDebugger.step(true).then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 4, `Could not step back: ${msg} - Try running 'target record-full' before stepping back`);
        });
    }
    stepInRequest(response, args) {
        this.miDebugger.step().then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 4, `Could not step in: ${msg}`);
        });
    }
    stepOutRequest(response, args) {
        this.miDebugger.stepOut().then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 5, `Could not step out: ${msg}`);
        });
    }
    nextRequest(response, args) {
        this.miDebugger.next().then(done => {
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 6, `Could not step over: ${msg}`);
        });
    }
    evaluateRequest(response, args) {
        const [threadId, level] = this.frameIdToThreadAndLevel(args.frameId);
        if (args.context == "watch" || args.context == "hover") {
            this.miDebugger.evalExpression(args.expression, threadId, level).then((res) => {
                response.body = {
                    variablesReference: 0,
                    result: res.result("value")
                };
                this.sendResponse(response);
            }, msg => {
                this.sendErrorResponse(response, 7, msg.toString());
            });
        }
        else {
            this.miDebugger.sendUserInput(args.expression, threadId, level).then(output => {
                if (typeof output == "undefined")
                    response.body = {
                        result: "",
                        variablesReference: 0
                    };
                else
                    response.body = {
                        result: JSON.stringify(output),
                        variablesReference: 0
                    };
                this.sendResponse(response);
            }, msg => {
                this.sendErrorResponse(response, 8, msg.toString());
            });
        }
    }
    gotoTargetsRequest(response, args) {
        this.miDebugger.goto(args.source.path, args.line).then(done => {
            response.body = {
                targets: [{
                        id: 1,
                        label: args.source.name,
                        column: args.column,
                        line: args.line
                    }]
            };
            this.sendResponse(response);
        }, msg => {
            this.sendErrorResponse(response, 16, `Could not jump: ${msg}`);
        });
    }
    gotoRequest(response, args) {
        this.sendResponse(response);
    }
    addSourceFileMapEntry(gdbCWD, ideCWD) {
        // if it looks like a Win32 path convert to "/"-style for comparisions and to all-lower-case
        if (ideCWD.indexOf("\\") != -1)
            ideCWD = ideCWD.replace(/\\/g, "/").toLowerCase();
        if (!ideCWD.endsWith("/"))
            ideCWD = ideCWD + "/";
        // ensure that we only replace complete paths
        if (gdbCWD.indexOf("\\") != -1)
            gdbCWD = gdbCWD.replace(/\\/g, "/").toLowerCase();
        if (!gdbCWD.endsWith("/"))
            gdbCWD = gdbCWD + "/";
        this.sourceFileMap.set(ideCWD, gdbCWD);
    }
    setSourceFileMap(configMap, fallbackGDB, fallbackIDE) {
        this.sourceFileMap = new Map();
        if (configMap === undefined) {
            this.addSourceFileMapEntry(fallbackGDB, fallbackIDE);
        }
        else {
            for (let [gdbPath, localPath] of Object.entries(configMap)) {
                this.addSourceFileMapEntry(gdbPath, localPath);
            }
        }
    }
}
exports.MI2DebugSession = MI2DebugSession;
function prettyStringArray(strings) {
    if (typeof strings == "object") {
        if (strings.length !== undefined)
            return strings.join(", ");
        else
            return JSON.stringify(strings);
    }
    else
        return strings;
}
//# sourceMappingURL=mibase.js.map