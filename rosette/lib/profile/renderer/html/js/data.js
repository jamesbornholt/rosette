// global state
var Data = {
    // populated by Racket
    events: [],
    infeasiblePCInfo: [],
    metadata: null,
    samples: [],
    config: {
        stream: false
    }
};
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
                for (var _b = 0, _c = data.infeasiblePCInfo; _b < _c.length; _b++) {
                    var ipt = _c[_b];
                    Data.infeasiblePCInfo.push(ipt);
                }
                if (init) {
                    updateCb(data.events, data.infeasiblePCInfo);
                }
                else {
                    initCb(data.events, data.infeasiblePCInfo);
                    init = true;
                }
            };
        }
        else {
            var scr = document.createElement("script");
            scr.onload = function () {
                initCb(Data.events, Data.infeasiblePCInfo);
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
