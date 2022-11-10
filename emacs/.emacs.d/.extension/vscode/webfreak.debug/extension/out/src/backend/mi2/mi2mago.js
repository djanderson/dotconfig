"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MI2_Mago = void 0;
const mi2lldb_1 = require("./mi2lldb");
const mi_parse_1 = require("../mi_parse");
class MI2_Mago extends mi2lldb_1.MI2_LLDB {
    getStack(startFrame, maxLevels, thread) {
        return new Promise((resolve, reject) => {
            const command = "stack-list-frames";
            this.sendCommand(command).then((result) => {
                const stack = result.resultRecords.results;
                const ret = [];
                const remaining = [];
                const addToStack = (element) => {
                    const level = mi_parse_1.MINode.valueOf(element, "frame.level");
                    const addr = mi_parse_1.MINode.valueOf(element, "frame.addr");
                    const func = mi_parse_1.MINode.valueOf(element, "frame.func");
                    const filename = mi_parse_1.MINode.valueOf(element, "file");
                    const file = mi_parse_1.MINode.valueOf(element, "fullname");
                    let line = 0;
                    const lnstr = mi_parse_1.MINode.valueOf(element, "line");
                    if (lnstr)
                        line = parseInt(lnstr);
                    const from = parseInt(mi_parse_1.MINode.valueOf(element, "from"));
                    ret.push({
                        address: addr,
                        fileName: filename || "",
                        file: file || "<unknown>",
                        function: func || from || "<unknown>",
                        level: level,
                        line: line
                    });
                };
                stack.forEach(element => {
                    if (element)
                        if (element[0] == "stack") {
                            addToStack(element[1]);
                        }
                        else
                            remaining.push(element);
                });
                if (remaining.length)
                    addToStack(remaining);
                resolve(ret);
            }, reject);
        });
    }
}
exports.MI2_Mago = MI2_Mago;
//# sourceMappingURL=mi2mago.js.map