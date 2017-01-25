// global state
let Data = {
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
function findUnique(key: string): Array<any> {
    var vals = [];
    for (let func of Data.functions) {
        for (let fcall of func["calls"]) {
            for (let val in fcall[key]) {
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
    return function() { // the callback invoked by contentready
        if (Data.config.stream) {
            let ws = new WebSocket('ws://localhost:8081');
            var init = false;
            ws.onmessage = (evt) => {
                let data = JSON.parse(evt.data);
                for (let e of data.events) Data.events.push(e);
                // console.log("new events:", data.events.length);
                initData();
                if (init) {
                    updateCb(data.events);
                } else {
                    initCb(data.events);
                    init = true;
                }
            }
        } else {
            let scr = document.createElement("script");
            scr.onload = () => {
                initData();
                initCb(Data.events);
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

function getFunctionName(func: string) {
    return func.split(" ")[0];
}