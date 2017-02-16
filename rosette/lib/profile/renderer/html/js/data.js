// global state
var Data = {
    // populated by Racket
    events: [],
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
                for (var _i = 0, _a = data.events; _i < _a.length; _i++) {
                    var e = _a[_i];
                    Data.events.push(e);
                }
                // console.log("new events:", data.events.length);
                initData();
                if (init) {
                    updateCb(data.events);
                }
                else {
                    initCb(data.events);
                    init = true;
                }
            };
        }
        else {
            var scr = document.createElement("script");
            scr.onload = function () {
                initData();
                initCb(Data.events);
            };
            scr.src = "data.json";
            document.head.appendChild(scr);
        }
    };
}
function eventsToGraph(events) {
    var computeMetrics = function (node) {
        var ret = {};
        var start = node["start"];
        var finish = node["finish"];
        for (var k in finish) {
            var exclKey = k + " (excl.)";
            var inclSum;
            if (start.hasOwnProperty(k)) {
                inclSum = finish[k] - start[k];
            }
            else {
                inclSum = finish[k];
            }
            var childSum = node["children"].map(function (c) { return c["metrics"][k]; }).reduce(function (a, b) { return a + b; }, 0);
            ret[k] = inclSum;
            ret[exclKey] = inclSum - childSum;
        }
        return ret;
    };
    var node;
    for (var _i = 0; _i < events.length; _i++) {
        var e = events[_i];
        if (e["type"] == "ENTER") {
            var evt = {
                "function": e["function"],
                "location": e["location"],
                "inputs": e["inputs"],
                "start": e["metrics"],
                "children": [],
                "parent": node
            };
            if (typeof node === "undefined") {
                evt.parent = evt;
            }
            else {
                node.children.push(evt);
            }
            node = evt;
        }
        else if (e["type"] == "EXIT") {
            var me = node;
            node["outputs"] = e["outputs"];
            node["finish"] = e["metrics"];
            node["metrics"] = computeMetrics(node);
            node = node.parent;
            delete me["parent"]; // circular reference
        }
    }
    return node;
}
function initData() {
    /*
    Data["graph"] = eventsToGraph(Data["events"]);
    // populate the "functions" aggregated list
    let worklist = [Data.graph];
    let functions = {};
    while (worklist.length > 0) {
        let node = worklist.pop();
        let func = node["function"];
        if (!functions.hasOwnProperty(func))
            functions[func] = {"name": func, "calls": []};
        functions[func]["calls"].push(node);
        if (node.hasOwnProperty("children"))
            worklist.push(...node["children"]);
    }
    Data.functions = Object.keys(functions).map((k) => functions[k]);

    // find inputs, outputs, metrics
    Data.inputs = findUnique("inputs");
    Data.outputs = findUnique("outputs");
    Data.metrics = findUnique("metrics");
    */
}
function getFunctionName(func) {
    return func.split(" ")[0];
}
var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
};
function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
    });
}
