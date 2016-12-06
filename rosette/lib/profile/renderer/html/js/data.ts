// global state
let Data = {
    metadata: null,
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
