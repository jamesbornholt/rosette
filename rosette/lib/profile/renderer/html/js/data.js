// global state
var Data = {
    // populated by Racket
    data: {
        nodes: [],
        edges: []
    },
    metadata: null,
    samples: [],
    config: {
        stream: false
    },
    // populated by initData
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
// create the onload event handler
function dataOnload(initCb, updateCb) {
    return function () {
        if (Data.config.stream) {
            var ws = new WebSocket('ws://localhost:8081');
            var init = false;
            ws.onmessage = function (evt) {
                var data = JSON.parse(evt.data);
                Data.data.nodes = data.nodes;
                Data.data.edges = data.edges;
                initData();
                if (init) {
                    updateCb();
                }
                else {
                    initCb();
                    init = true;
                }
            };
        }
        else {
            var scr = document.createElement("script");
            scr.onload = function () {
                initData();
                initCb();
            };
            scr.src = "data.json";
            document.head.appendChild(scr);
        }
    };
}
function initData() {
    // reconstruct the graph from the list of nodes/edges in data
    for (var _i = 0, _a = Data.data.nodes; _i < _a.length; _i++) {
        var n = _a[_i];
        n.children = []; // every node needs a list of children
    }
    for (var _b = 0, _c = Data.data.edges; _b < _c.length; _b++) {
        var e = _c[_b];
        var a = e[0], b = e[1];
        Data.data.nodes[a].children.push(Data.data.nodes[b]);
    }
    Data.graph = Data.data.nodes[0];
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