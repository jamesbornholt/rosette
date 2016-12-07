// global state
var Data = {
    metadata: null,
    inputs: [],
    outputs: [],
    metrics: [],
    functions: [],
    graph: null
};
// collate all unique entries in the profile data
function findUnique(key) {
    var vals = [];
    for (var _i = 0, _a = Data.functions; _i < _a.length; _i++) {
        var func = _a[_i];
        for (var _b = 0, _c = func["calls"]; _b < _c.length; _b++) {
            var fcall = _c[_b];
            for (var val in fcall[key]) {
                if (vals.indexOf(val) == -1) {
                    vals.push(val);
                }
            }
        }
    }
    return vals;
}
function initData() {
    // populate the "functions" aggregated list
    var worklist = [Data.graph];
    var functions = {};
    while (worklist.length > 0) {
        var node = worklist.pop();
        var func = node["function"];
        if (!functions.hasOwnProperty(func))
            functions[func] = { "name": func, "calls": [] };
        functions[func]["calls"].push(node);
        if (node.hasOwnProperty("children"))
            worklist.push.apply(worklist, node["children"]);
    }
    Data.functions = Object.keys(functions).map(function (k) { return functions[k]; });
    // find inputs, outputs, metrics
    Data.inputs = findUnique("inputs");
    Data.outputs = findUnique("outputs");
    Data.metrics = findUnique("metrics");
}
function getFunctionName(func) {
    return func.split(" ")[0];
}
//# sourceMappingURL=data.js.map