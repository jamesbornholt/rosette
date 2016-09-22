// global state
var Profile = {
    inputs: [],
    outputs: [],
    metrics: [],
    data: null,
    entries: [],
    selected: {
        input: null,
        output: null,
        entry: null
    },
    sorter: null
};
// collate all unique entries in the profile data
function findUnique(key) {
    var vals = [];
    for (var _i = 0, _a = Profile.data["functions"]; _i < _a.length; _i++) {
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
// update a select element to contain the given options, preserving the
// currently selected option if possible
function updateSelect(select, lst) {
    var currentIndex = select.selectedIndex;
    var currentValue = currentIndex == -1 ? null : select.value;
    for (var _i = 0, _a = select.childNodes; _i < _a.length; _i++) {
        var opt = _a[_i];
        select.removeChild(opt);
    }
    var newIndex = -1;
    for (var _b = 0, lst_1 = lst; _b < lst_1.length; _b++) {
        var x = lst_1[_b];
        var opt = document.createElement("option");
        opt.value = x;
        opt.innerHTML = x;
        select.insertAdjacentElement('beforeend', opt);
        if (x == currentValue) {
            newIndex = select.childNodes.length - 1;
        }
    }
    if (newIndex != -1) {
        select.selectedIndex = newIndex;
    }
}
function init() {
    Profile.inputs = findUnique("inputs");
    Profile.outputs = findUnique("outputs");
    Profile.metrics = findUnique("metrics");
    // update the profile source info
    document.getElementById("source").innerHTML = Profile.data.source;
    document.getElementById("form").innerHTML = Profile.data.form;
    // populate available inputs
    var input_select = document.getElementById("input");
    updateSelect(input_select, Profile.inputs);
    // populate available outputs
    var output_select = document.getElementById("output");
    updateSelect(output_select, Profile.outputs.concat(Profile.metrics));
    // hook the input/output dropdowns to re-render the table
    input_select.addEventListener('change', renderTable);
    output_select.addEventListener('change', renderTable);
    // render the initial table
    Profile.sorter = new Tablesort(document.getElementById("profile"), { descending: true });
    renderTable();
}
function getSelectedOption(elt) {
    return elt.options[elt.selectedIndex].value;
}
function selectProfilePoints(data, input, output) {
    var pts = [];
    for (var _i = 0, data_1 = data; _i < data_1.length; _i++) {
        var fcall = data_1[_i];
        if (!fcall["inputs"].hasOwnProperty(input))
            continue;
        var i = fcall["inputs"][input];
        var o;
        if (fcall["outputs"].hasOwnProperty(output))
            o = fcall["outputs"][output];
        else if (fcall["metrics"].hasOwnProperty(output))
            o = fcall["metrics"][output];
        else
            continue;
        pts.push([i, o, fcall["location"]]);
    }
    return pts;
}
function generateProfile(input, output) {
    var entries = [];
    for (var _i = 0, _a = Profile.data["functions"]; _i < _a.length; _i++) {
        var func = _a[_i];
        var pts = selectProfilePoints(func.calls, input, output);
        var reg = regression('power', pts);
        var sum = pts.map(function (v) { return v[1]; }).reduce(function (a, b) { return a + b; }, 0);
        entries.push({ "name": func.name, "points": pts, "fit": reg, "calls": pts.length, "output": sum / pts.length });
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
};
function escapeHtml(string) {
    return String(string).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
    });
}
function makeCell(str, row) {
    var elt = document.createElement("td");
    elt.innerHTML = escapeHtml(str);
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
    var entries = generateProfile(Profile.selected.input, Profile.selected.output);
    // sort in decreasing R^2 order, with NaNs last
    entries.sort(function (a, b) {
        if (!isFinite(b.fit.r2 - a.fit.r2))
            return !isFinite(a.fit.r2) ? 1 : -1;
        else
            return b.fit.r2 - a.fit.r2;
    });
    // remove all table rows
    for (var _i = 0, _a = document.querySelectorAll("table#profile tbody tr"); _i < _a.length; _i++) {
        var node = _a[_i];
        node.parentNode.removeChild(node);
    }
    var tbody = document.querySelectorAll("table#profile tbody")[0];
    Profile.entries = [];
    // render new table rows
    for (var _b = 0, entries_1 = entries; _b < entries_1.length; _b++) {
        var entry_1 = entries_1[_b];
        // render the row
        var row = document.createElement("tr");
        // 1. function name
        var func = makeCell(entry_1.name.split(" ")[0], row);
        func.title = entry_1.name.indexOf(" ") > -1 ?
            entry_1.name.slice(entry_1.name.indexOf(" ") + 1) :
            "<no source info>";
        // 2. curve
        var fit = makeCell(entry_1.fit.string, row);
        fit.dataset["sort"] = entry_1.fit.equation[1].toFixed(2);
        // 3. r^2
        makeCell(isNaN(entry_1.fit.r2) ? "-" : entry_1.fit.r2.toFixed(2), row);
        // 4. # calls
        makeCell(entry_1.calls, row);
        // 5. avg output
        makeCell(entry_1.output.toFixed(2), row);
        // store the entry in the profile state
        entry_1.row = row;
        Profile.entries.push(entry_1);
        row["entry"] = entry_1;
        // set up event listener for clicks on this row to change graph
        row.addEventListener('click', profileEntryClick);
        tbody.insertAdjacentElement('beforeend', row);
    }
    // maintain the selection if possible, otherwise select the
    // first element in the list
    var new_selection = null;
    if (Profile.selected.entry != null) {
        for (var _c = 0, _d = Profile.entries; _c < _d.length; _c++) {
            var entry = _d[_c];
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
function selectEntry(entry) {
    Profile.selected.entry = entry;
    // highlight the selected row
    for (var _i = 0, _a = document.querySelectorAll('.selected'); _i < _a.length; _i++) {
        var elt = _a[_i];
        elt.classList.remove('selected');
    }
    if (entry != null) {
        entry.row.classList.add('selected');
        renderGraph(Profile.selected.input, Profile.selected.output, entry);
    }
}
function renderGraph(input, output, entry) {
    var old_graph = document.getElementById("graph");
    var graph = document.createElement("div");
    graph.id = "graph";
    // gather the data into a list of the form [{x: x, y: y}]
    var data = [];
    for (var _i = 0, _a = entry.points; _i < _a.length; _i++) {
        var pt = _a[_i];
        data.push({ "x": pt[0], "y": pt[1], "location": pt[2] });
    }
    // render the vega spec
    var spec = {
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
    vg.embed(graph, { mode: "vega-lite", spec: spec }, function (err, res) { });
    // swap out the graph element
    old_graph.parentNode.replaceChild(graph, old_graph);
}
function profileEntryClick(evt) {
    var row = this;
    selectEntry(row.entry);
}
document.addEventListener("DOMContentLoaded", init);
//# sourceMappingURL=profile.js.map