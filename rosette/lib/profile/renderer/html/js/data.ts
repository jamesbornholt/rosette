// global state
let Data = {
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
    return function() { // the callback invoked by contentready
        if (Data.config.stream) {
            let ws = new WebSocket('ws://localhost:8081');
            var init = false;
            ws.onmessage = (evt) => {
                let data = JSON.parse(evt.data);
                for (let e of data.events) Data.events.push(e);
                for (let ipt of data.infeasiblePCInfo) Data.infeasiblePCInfo.push(ipt);
                if (init) {
                    updateCb(data.events, data.infeasiblePCInfo);
                } else {
                    initCb(data.events, data.infeasiblePCInfo);
                    init = true;
                }
            }
        } else {
            let scr = document.createElement("script");
            scr.onload = () => {
                initCb(Data.events, Data.infeasiblePCInfo);
            }
            scr.src = "data.json";
            document.head.appendChild(scr);
        }
    }
}

function eventsToGraph(events: Array<any>) {
    let computeMetrics = (node) => {
        let ret = {};
        let start = node["start"];
        let finish = node["finish"];
        for (let k in finish) {
            let exclKey = k + " (excl.)";
            var inclSum;
            if (start.hasOwnProperty(k)) {
                inclSum =  finish[k] - start[k];
            } else {
                inclSum = finish[k];
            }
            let childSum = node["children"].map(c => c["metrics"][k]).reduce((a, b) => a + b, 0);
            ret[k] = inclSum;
            ret[exclKey] = inclSum - childSum;
        }
        return ret;
    };

    var node;
    for (let e of events) {
        if (e["type"] == "ENTER") {
            let evt = {
                "function": e["function"],
                "location": e["location"],
                "inputs": e["inputs"],
                "start": e["metrics"],
                "children": [],
                "parent": node
            };
            if (typeof node === "undefined") {
                evt.parent = evt;
            } else {
                node.children.push(evt);
            }
            node = evt;
        } else if (e["type"] == "EXIT") {
            let me = node;
            node["outputs"] = e["outputs"];
            node["finish"] = e["metrics"];
            node["metrics"] = computeMetrics(node);
            node = node.parent;
            delete me["parent"]; // circular reference
        }
    }

    return node;
}

function getFunctionName(func: string) {
    return func.split(" ")[0];
}

var entityMap = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
}
function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
    });
}
