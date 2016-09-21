declare var profile_data;  // profile data supplied by data.json
declare var vg;            // vega
declare var regression;    // regression.js

// convince TS that document.querySelectorAll can be an array
interface NodeListOf<TNode extends Node> extends Array<TNode> {}

function findUnique(profile, key) {
    var vals = [];
    for (let func of profile["functions"]) {
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

function init() {
    let inputs = findUnique(profile_data, "inputs");
    let outputs = findUnique(profile_data, "outputs");
    let metrics = findUnique(profile_data, "metrics");
    
    document.getElementById("source").innerHTML = profile_data.source;
    document.getElementById("form").innerHTML = profile_data.form;

    let updateSelect = function(select, lst) {
        for (let x of lst) {
            let opt = document.createElement("option");
            opt.value = x;
            opt.innerHTML = x;
            select.insertAdjacentElement('beforeend', opt);
        }
    }

    let input_select = document.getElementById("input");
    let output_select = document.getElementById("output");

    updateSelect(input_select, inputs);
    updateSelect(output_select, outputs.concat(metrics));

    input_select.addEventListener('change', renderTable);
    output_select.addEventListener('change', renderTable);

    renderTable();
}

function getSelectedOption(elt) {
    return elt.options[elt.selectedIndex].value;
}

function selectProfilePoints(data, input, output) {
    var pts = [];
    for (let fcall of data) {
        if (!fcall["inputs"].hasOwnProperty(input)) continue;
        var i = fcall["inputs"][input];
        var o;
        if (fcall["outputs"].hasOwnProperty(output))
            o = fcall["outputs"][output];
        else if (fcall["metrics"].hasOwnProperty(output))
            o = fcall["metrics"][output];
        else continue;
        pts.push([i, o]);
    }
    return pts;
}

function generateProfile(input, output) {
    var entries = [];
    for (let func of profile_data["functions"]) {
        let pts = selectProfilePoints(func.calls, input, output);
        let reg_power = regression('power', pts);
        let reg_linear = regression('linear', pts);
        let reg_best = reg_power.r2 > reg_linear.r2 ? reg_power : reg_linear;
        entries.push({"name": func.name, "points": pts, "fit": reg_best, "calls": pts.length});
    }
    return entries;
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

function makeCell(str, row) {
    let elt = document.createElement("td");
    elt.innerHTML = escapeHtml(str);
    row.insertAdjacentElement('beforeend', elt);
    return elt;
}

function renderTable() {
    let input = getSelectedOption(document.getElementById("input"));
    let output = getSelectedOption(document.getElementById("output"));

    let entries = generateProfile(input, output);
    // sort in decreasing R^2 order, with NaNs last
    entries.sort(function(a, b) {
        if (!isFinite(b.fit.r2 - a.fit.r2))
            return !isFinite(a.fit.r2) ? 1 : -1;
        else return b.fit.r2 - a.fit.r2;
    });

    let table = document.getElementById("profile");
    for (let node of document.querySelectorAll("table tr:not(.header)")) {
        node.parentNode.removeChild(node);
    }
    for (let entry of entries) {
        let row = document.createElement("tr");
        let func = makeCell(entry.name.split(" ")[0], row);
        func.title = entry.name.indexOf(" ") > -1 ? 
                      entry.name.slice(entry.name.indexOf(" ") + 1) : 
                      "<no source info>";
        makeCell(entry.fit.string, row);
        makeCell(isNaN(entry.fit.r2) ? "-" : entry.fit.r2.toFixed(2), row);
        makeCell(entry.calls, row);

        // set up event listener for graph
        row["chartData"] = entry;
        row.addEventListener('click', profileEntryClick);
        table.insertAdjacentElement('beforeend', row);
    }

    if (entries.length > 0) {
        renderGraph(input, output, entries[0], table.childNodes[1]);
    }
}

function renderGraph(input, output, entry, row) {
    let old_graph = document.getElementById("graph");
    let graph = document.createElement("div");
    graph.id = "graph";

    let data = [];

    for (let pt of entry.points) {
        data.push({"x": pt[0], "y": pt[1]});
    }

    let spec = {"data": {"values": data}, "mark": "point",
                "width": 400, "height": 400,
                "encoding": {"x": {"field": "x", "type": "quantitative", "axis": {"title": "input " + input}}, 
                             "y": {"field": "y", "type": "quantitative", "axis": {"title": "output " + output}}}};
    vg.embed(graph, {mode: "vega-lite", spec: spec}, function(err, res) {});

    for (var elt of document.querySelectorAll('.selected')) {
        elt.classList.remove('selected');
    }
    row.classList.add('selected');

    // swap out the graph
    old_graph.parentNode.replaceChild(graph, old_graph);
}

function profileEntryClick(evt) {
    let row = this;
    let input = getSelectedOption(document.getElementById("input"));
    let output = getSelectedOption(document.getElementById("output"));

    renderGraph(input, output, row.chartData, row);
}

document.addEventListener("DOMContentLoaded", init);
