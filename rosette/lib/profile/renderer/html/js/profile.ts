declare var vg;            // vega
declare var regression;    // regression.js
declare var Tablesort;     // tablesort.js

// convince TS that document.querySelectorAll can be an array
interface NodeList extends Array<Node> {}

let default_input = "heap-size";
let default_output = "term-count";

// global state
let Profile = {
    inputs: [],
    outputs: [],
    metrics: [],
    data: null,
    entries: [],
    selected: {
        input: null,
        output: null,
        entry: null,
    },
    sorter: null,
};

// collate all unique entries in the profile data
function findUnique(key: string): Array<any> {
    var vals = [];
    for (let func of Profile.data["functions"]) {
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

// update a select element to contain the given options, preserving the
// currently selected option if possible
function updateSelect(select: HTMLSelectElement, lst: Array<any>, defaultOption: string) {
    let currentIndex = select.selectedIndex;
    let currentValue = currentIndex == -1 ? null : select.value;
    for (let opt of select.childNodes) {
        select.removeChild(opt);
    }
    var newIndex = -1;
    var defaultIndex = -1;
    lst.sort();
    for (let x of lst) {
        let opt = document.createElement("option");
        opt.value = x;
        opt.innerHTML = x;
        select.insertAdjacentElement('beforeend', opt);
        if (x == currentValue) {
            newIndex = select.childNodes.length - 1;
        }
        if (x == defaultOption) {
            defaultIndex = select.childNodes.length - 1;
        }
    }
    if (newIndex != -1) {
        select.selectedIndex = newIndex;
    } else if (defaultIndex != -1) {
        select.selectedIndex = defaultIndex;
    }
}

function init() {
    Profile.inputs = findUnique("inputs");
    Profile.outputs = findUnique("outputs");
    Profile.metrics = findUnique("metrics");

    // update the profile source info
    document.getElementById("name").innerHTML = Profile.data.name;
    document.getElementById("source").innerHTML = Profile.data.source;
    document.getElementById("form").innerHTML = Profile.data.form;
    document.getElementById("time").innerHTML = Profile.data.time;
    document.title = "Profile: " + Profile.data.name;

    // populate available inputs
    let input_select = document.getElementById("input");
    updateSelect(input_select as HTMLSelectElement, Profile.inputs, default_input);
    // populate available outputs
    let output_select = document.getElementById("output");
    updateSelect(output_select as HTMLSelectElement, Profile.outputs.concat(Profile.metrics), default_output);
    // hook the input/output dropdowns to re-render the table
    input_select.addEventListener('change', renderTable);
    output_select.addEventListener('change', renderTable);

    // render the initial table
    Profile.sorter = new Tablesort(document.getElementById("profile"), {descending: true});
    renderTable();


    findImportantEntries();
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
        pts.push([i, o, fcall["location"]]);
    }
    return pts;
}

function findBestFit(pts) {
    let shifted = pts.map((v) => [v[0], v[1]+1]);
    let reg_pwr = regression('power', shifted);
    let reg_lin = regression('linear', pts);
    if (reg_pwr.r2 > reg_lin.r2) {
        let [a, b] = reg_pwr.equation;
        reg_pwr.string = "y = " + a.toPrecision(2) + "x<sup>" + b.toFixed(2) + "</sup>";
        reg_pwr["sort"] = b;
        return reg_pwr;
    } else {
        let [a, b] = reg_lin.equation;
        reg_lin.string = "y = " + a.toPrecision(2) + "x + " + b.toFixed(2);
        reg_lin["sort"] = 1;
        return reg_lin;
    }
}

function generateProfile(input, output) {
    var entries = [];
    for (let func of Profile.data["functions"]) {
        let pts = selectProfilePoints(func.calls, input, output);
        let reg = findBestFit(pts);
        let sum = pts.map((v) => v[1]).reduce((a,b) => a+b, 0);
        entries.push({"name": func.name, "points": pts, "fit": reg, "calls": pts.length, "output": sum/pts.length});
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

function makeCell(str, row, escape = true) {
    let elt = document.createElement("td");
    elt.innerHTML = escape? escapeHtml(str) : str;
    row.insertAdjacentElement('beforeend', elt);
    return elt;
}

function renderTable() {
    // get the selected input/output
    Profile.selected.input = getSelectedOption(document.getElementById("input"));
    Profile.selected.output = getSelectedOption(document.getElementById("output"));

    // update the output column
    document.getElementById("output-col").innerHTML = "Avg " + Profile.selected.output;

    // generate the profile entries using these metrics
    let entries = generateProfile(Profile.selected.input, Profile.selected.output);
    // sort in decreasing R^2 order, with NaNs last
    entries.sort(function(a, b) {
        if (!isFinite(b.fit.r2 - a.fit.r2))
            return !isFinite(a.fit.r2) ? 1 : -1;
        else return b.fit.r2 - a.fit.r2;
    });

    // remove all table rows
    for (let node of document.querySelectorAll("table#profile tbody tr")) {
        node.parentNode.removeChild(node);
    }
    let tbody = document.querySelectorAll("table#profile tbody")[0] as HTMLElement;
    Profile.entries = [];

    // render new table rows
    for (let entry of entries) {
        // render the row
        let row = document.createElement("tr");
        // 1. function name
        let func = makeCell(entry.name.split(" ")[0], row);
        func.title = entry.name.indexOf(" ") > -1 ? 
                      entry.name.slice(entry.name.indexOf(" ") + 1) : 
                      "<no source info>";
        func.classList.add("code");
        // 2. curve
        let fit = entry.fit;
        let fitCell = makeCell(isNaN(fit.equation[0]) ? "-" : fit.string, row, false);
        fitCell.dataset["sort"] = fit.sort;
        // 3. r^2
        let r2Cell = makeCell(isNaN(entry.fit.r2) ? "-" : entry.fit.r2.toFixed(2), row);
        r2Cell.classList.add("numeric");
        // 4. # calls
        let callCell = makeCell(entry.calls, row);
        callCell.classList.add("numeric");
        // 5. avg output
        let avgCell = makeCell(entry.output.toFixed(2), row);
        avgCell.classList.add("numeric");

        // store the entry in the profile state
        entry.row = row;
        Profile.entries.push(entry);
        row["entry"] = entry;

        // set up event listener for clicks on this row to change graph
        row.addEventListener('click', profileEntryClick);

        tbody.insertAdjacentElement('beforeend', row);
    }

    // maintain the selection if possible, otherwise select the
    // first element in the list
    var new_selection = null;
    if (Profile.selected.entry != null) {
        for (var entry of Profile.entries) {
            if (entry.name == Profile.selected.entry.name) {
                new_selection = entry;
                break;
            }
        }
    }
    if (new_selection == null && Profile.entries.length > 0) {
        new_selection = Profile.entries[0];
    }
    selectEntry(new_selection);

    // sort the table
    Profile.sorter.refresh();
}

function renderSubTable(input, output, entry) {
    // aggregate by callsite
    let callSites = {};
    for (let pt of entry.points) {
        if (!callSites.hasOwnProperty(pt[2]))
            callSites[pt[2]] = [];
        callSites[pt[2]].push(pt);
    }
    let callSiteNames = Object.keys(callSites);
    callSiteNames.sort();

    // update the output column
    document.getElementById("suboutput-col").innerHTML = "Avg " + Profile.selected.output;

    // update the header
    for (let elt of document.querySelectorAll(".selected-function")) {
        elt.innerHTML = escapeHtml(entry.name.split(" ")[0]);
    }

    // remove existing rows
    for (let node of document.querySelectorAll("table#subprofile tbody tr")) {
        node.parentNode.removeChild(node);
    }
    let tbody = document.querySelectorAll("table#subprofile tbody")[0] as HTMLElement;

    // render row for each callsite
    for (let callSite of callSiteNames) {
        let row = document.createElement("tr");
        let pts = callSites[callSite];
        // fit the data
        let reg = findBestFit(pts);
        let sum = pts.map((v) => v[1]).reduce((a,b) => a+b, 0);

        // 1. call site name
        makeCell(callSite, row);
        // 2. fit
        let fit = makeCell(isNaN(reg.equation[0]) ? "-" : reg.string, row, false);
        // 3. r^2
        makeCell(isNaN(reg.r2) ? "-" : reg.r2.toFixed(2), row);
        // 4. # calls
        makeCell(pts.length, row);
        // 5. avg output
        makeCell((sum / pts.length).toFixed(2), row);

        tbody.insertAdjacentElement('beforeend', row);
    }
}

function findImportantEntries() {
    // aggregate all fcalls by callsite
    let callSites = {};
    for (let func of Profile.data["functions"]) {
        if (!callSites.hasOwnProperty(func.name))
            callSites[func.name] = {};
        for (let fcall of func["calls"]) {
            if (!callSites[func.name].hasOwnProperty(fcall.location))
                callSites[func.name][fcall.location] = [];
            callSites[func.name][fcall.location].push(fcall);
        }
    }
    
    let mergeCounts = [];
    let heapSizes = [];
    for (let func in callSites) {
        for (let loc in callSites[func]) {
            let myMergeCounts = [];
            let ioHeapSizes = [];
            for (let fcall of callSites[func][loc]) {
                if (fcall.metrics.hasOwnProperty("merge-count (excl.)")) {
                    myMergeCounts.push(fcall.metrics["merge-count (excl.)"]);
                }
                if (fcall.inputs.hasOwnProperty("heap-size")
                      && fcall.outputs.hasOwnProperty("heap-size")) {
                    ioHeapSizes.push([fcall.inputs["heap-size"], fcall.outputs["heap-size"]]);
                }
            }
            if (myMergeCounts.length > 0) {
                let mcSum = myMergeCounts.reduce((a,b) => a+b, 0);
                let mcAvg = mcSum / myMergeCounts.length;
                mergeCounts.push({func: func, loc: loc, mc: mcAvg});
            }
            let myMax = 0.0;
            for (let ioHeapSize of ioHeapSizes) {
                if (ioHeapSize[0] > 0) {
                    let ratio = ioHeapSize[1] / ioHeapSize[0];
                    if (ratio > myMax) {
                        myMax = ratio;
                    }
                }
            }
            heapSizes.push({func: func, loc: loc, ratio: myMax});
        }
    }

    let highlights = [];

    if (heapSizes.length > 0) {
        heapSizes.sort((a,b) => a.ratio < b.ratio ? 1 : (a.ratio > b.ratio ? -1 : 0));
        let top5pct = Math.min(Math.ceil(heapSizes.length / 20), 4);
        for (var i = 0; i <= top5pct; i++) {
            let entry = heapSizes[i];
            if (entry.ratio >= 2) {
                highlights.push({func: entry.func, loc: entry.loc, reason: "heap size blowup (" + entry.ratio.toFixed(2) + ")"});
            }
        }
    }
    if (mergeCounts.length > 0) {
        mergeCounts.sort((a,b) => a.mc < b.mc ? 1 : (a.mc > b.mc ? -1 : 0));
        let top5pct = Math.min(Math.ceil(mergeCounts.length / 20), 4);
        for (var i = 0; i <= top5pct; i++) {
            let entry = mergeCounts[i];
            if (entry.mc > 10) {
                highlights.push({func: entry.func, loc: entry.loc, reason: "high merge count (" + entry.mc.toFixed(2) + ")"});
            }
        }
    }

    let tbody = document.querySelector("#important tbody");
    for (let hi of highlights) {
        let row = document.createElement("tr");
        // 1. name
        let [name, ...rest] = hi.func.split(" ");
        let body = "<code>" + escapeHtml(name) + "</code> (called at " + escapeHtml(hi.loc) + ")"; 
        let func = makeCell(body, row, false);
        func.title = hi.func.indexOf(" ") > -1 ? 
                     hi.func.slice(hi.func.indexOf(" ") + 1) : 
                     "<no source info>";
        // 2. reason
        makeCell(hi.reason, row);

        tbody.insertAdjacentElement("beforeend", row);
    }
    console.log("highlights", highlights);
}

function selectEntry(entry) {
    Profile.selected.entry = entry;
    // highlight the selected row
    for (var elt of document.querySelectorAll('.selected')) {
        elt.classList.remove('selected');
    }
    if (entry != null) {
        entry.row.classList.add('selected');
        renderGraph(Profile.selected.input, Profile.selected.output, entry);
        renderSubTable(Profile.selected.input, Profile.selected.output, entry);
    }
}

function renderGraph(input, output, entry) {
    let old_graph = document.getElementById("graph");
    let graph = document.createElement("div");
    graph.id = "graph";

    // gather the data into a list of the form [{x: x, y: y}]
    let data = [];
    for (let pt of entry.points) {
        data.push({"x": pt[0], "y": pt[1], "location": pt[2]});
    }

    // render the vega spec
    let spec = {
        "data": {
            "values": data
        },
        "mark": "point",
        "width": 400,
        "height": 400,
        "encoding": {
            "x": {
                "field": "x",
                "type": "quantitative",
                "axis": {
                    "title": "input " + input
                }
            },
            "y": {
                "field": "y",
                "type": "quantitative",
                "axis": {
                    "title": "output " + output
                }
            },
            "color": {
                "field": "location",
                "type": "nominal",
                "legend": {
                    "title": "call site"
                }
            }
        }
    };
    vg.embed(graph, {mode: "vega-lite", spec: spec}, function(err, res) {});

    // swap out the graph element
    old_graph.parentNode.replaceChild(graph, old_graph);
}

function profileEntryClick(evt) {
    let row = this;
    selectEntry(row.entry);
}

document.addEventListener("DOMContentLoaded", init);
