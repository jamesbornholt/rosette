function findUnique(profile, key) {
    var vals = [];
    for (var _i = 0, _a = profile["functions"]; _i < _a.length; _i++) {
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
function init() {
    var inputs = findUnique(profile_data, "inputs");
    var outputs = findUnique(profile_data, "outputs");
    var metrics = findUnique(profile_data, "metrics");
    document.getElementById("source").innerHTML = profile_data.source;
    document.getElementById("form").innerHTML = profile_data.form;
    var updateSelect = function (select, lst) {
        for (var _i = 0, lst_1 = lst; _i < lst_1.length; _i++) {
            var x = lst_1[_i];
            var opt = document.createElement("option");
            opt.value = x;
            opt.innerHTML = x;
            select.insertAdjacentElement('beforeend', opt);
        }
    };
    var input_select = document.getElementById("input");
    var output_select = document.getElementById("output");
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
        pts.push([i, o]);
    }
    return pts;
}
function generateProfile(input, output) {
    var entries = [];
    for (var _i = 0, _a = profile_data["functions"]; _i < _a.length; _i++) {
        var func = _a[_i];
        var pts = selectProfilePoints(func.calls, input, output);
        var reg_power = regression('power', pts);
        var reg_linear = regression('linear', pts);
        var reg_best = reg_power.r2 > reg_linear.r2 ? reg_power : reg_linear;
        entries.push({ "name": func.name, "points": pts, "fit": reg_best, "calls": pts.length });
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
    var input = getSelectedOption(document.getElementById("input"));
    var output = getSelectedOption(document.getElementById("output"));
    var entries = generateProfile(input, output);
    // sort in decreasing R^2 order, with NaNs last
    entries.sort(function (a, b) {
        if (!isFinite(b.fit.r2 - a.fit.r2))
            return !isFinite(a.fit.r2) ? 1 : -1;
        else
            return b.fit.r2 - a.fit.r2;
    });
    var table = document.getElementById("profile");
    for (var _i = 0, _a = document.querySelectorAll("table tr:not(.header)"); _i < _a.length; _i++) {
        var node = _a[_i];
        node.parentNode.removeChild(node);
    }
    for (var _b = 0, entries_1 = entries; _b < entries_1.length; _b++) {
        var entry = entries_1[_b];
        var row = document.createElement("tr");
        var func = makeCell(entry.name.split(" ")[0], row);
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
    var old_graph = document.getElementById("graph");
    var graph = document.createElement("div");
    graph.id = "graph";
    var data = [];
    for (var _i = 0, _a = entry.points; _i < _a.length; _i++) {
        var pt = _a[_i];
        data.push({ "x": pt[0], "y": pt[1] });
    }
    var spec = { "data": { "values": data }, "mark": "point",
        "width": 400, "height": 400,
        "encoding": { "x": { "field": "x", "type": "quantitative", "axis": { "title": "input " + input } },
            "y": { "field": "y", "type": "quantitative", "axis": { "title": "output " + output } } } };
    vg.embed(graph, { mode: "vega-lite", spec: spec }, function (err, res) { });
    for (var _b = 0, _c = document.querySelectorAll('.selected'); _b < _c.length; _b++) {
        var elt = _c[_b];
        elt.classList.remove('selected');
    }
    row.classList.add('selected');
    // swap out the graph
    old_graph.parentNode.replaceChild(graph, old_graph);
}
function profileEntryClick(evt) {
    var row = this;
    var input = getSelectedOption(document.getElementById("input"));
    var output = getSelectedOption(document.getElementById("output"));
    renderGraph(input, output, row.chartData, row);
}
document.addEventListener("DOMContentLoaded", init);
//# sourceMappingURL=profile.js.map