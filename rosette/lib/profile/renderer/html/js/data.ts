// global state
let Data = {
    // populated by Racket
    data: {
        nodes: [],
        edges: []
    },
    metadata: null,
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
            var scr;
            let intervalHandler = () => {
                console.log("handler...");
                if (scr) scr.parentNode.removeChild(scr);
                scr = document.createElement("script");
                scr.onload = () => { initData(); updateCb(); }
                scr.src = "stream.json";
                document.head.appendChild(scr);
            };
            scr = document.createElement("script");
            scr.onload = () => {
                initData();
                initCb();
                window.setInterval(intervalHandler, 2000);
            }
            scr.src = "stream.json";
            document.head.appendChild(scr);
        } else {
            let scr = document.createElement("script");
            scr.onload = () => {
                initData();
                initCb();
            }
            scr.src = "data.json";
            document.head.appendChild(scr);
        }
    }
}

function initData() {
    // reconstruct the graph from the list of nodes/edges in data
    for (let n of Data.data.nodes) {
        n.children = [];  // every node needs a list of children
    }
    for (let e of Data.data.edges) {
        let [a, b] = e;
        Data.data.nodes[a].children.push(Data.data.nodes[b]);
    }
    Data.graph = Data.data.nodes[0];
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
}

function getFunctionName(func: string) {
    return func.split(" ")[0];
}