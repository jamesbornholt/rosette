// global state
let Data = {
    // populated by Racket
    data: {
        nodes: [],
        edges: []
    },
    metadata: null,
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