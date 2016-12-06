/// <reference path="data.ts" />
var profile;
(function (profile) {
    var defaultInput = "heap-size";
    var defaultOutput = "term-count";
    // global state
    var Profile = {
        entries: [],
        selected: {
            input: null,
            output: null,
            entry: null
        },
        sorter: null
    };
    // update a select element to contain the given options, preserving the
    // currently selected option if possible
    function updateSelect(select, lst, defaultOption) {
        var currentIndex = select.selectedIndex;
        var currentValue = currentIndex == -1 ? null : select.value;
        for (var _i = 0, _a = select.childNodes; _i < _a.length; _i++) {
            var opt = _a[_i];
            select.removeChild(opt);
        }
        var newIndex = -1;
        var defaultIndex = -1;
        lst.sort();
        for (var _b = 0, lst_1 = lst; _b < lst_1.length; _b++) {
            var x = lst_1[_b];
            var opt = document.createElement("option");
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
        }
        else if (defaultIndex != -1) {
            select.selectedIndex = defaultIndex;
        }
    }
    function init() {
        // Initialize the profile data
        initData();
        // update the profile source info
        document.getElementById("name").innerHTML = Data.metadata.name;
        document.getElementById("source").innerHTML = Data.metadata.source;
        document.getElementById("form").innerHTML = Data.metadata.form;
        document.getElementById("time").innerHTML = Data.metadata.time;
        document.title = "Profile: " + Data.metadata.name;
        // populate available inputs
        var input_select = document.getElementById("input");
        updateSelect(input_select, Data.inputs, defaultInput);
        // populate available outputs
        var output_select = document.getElementById("output");
        updateSelect(output_select, Data.outputs.concat(Data.metrics), defaultOutput);
        // hook the input/output dropdowns to re-render the table
        input_select.addEventListener('change', renderTable);
        output_select.addEventListener('change', renderTable);
        // render the initial table
        Profile.sorter = new Tablesort(document.getElementById("profile"), { descending: true });
        renderTable();
        // highlight important entries
        findImportantEntries();
    }
    profile.init = init;
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
    function findBestFit(pts) {
        var shifted = pts.map(function (v) { return [v[0], v[1] + 1]; });
        var reg_pwr = regression('power', shifted);
        var reg_lin = regression('linear', pts);
        if (reg_pwr.r2 > reg_lin.r2) {
            var _a = reg_pwr.equation, a = _a[0], b = _a[1];
            reg_pwr.string = "y = " + a.toPrecision(2) + "x<sup>" + b.toFixed(2) + "</sup>";
            reg_pwr["sort"] = b;
            return reg_pwr;
        }
        else {
            var _b = reg_lin.equation, a = _b[0], b = _b[1];
            reg_lin.string = "y = " + a.toPrecision(2) + "x + " + b.toFixed(2);
            reg_lin["sort"] = 1;
            return reg_lin;
        }
    }
    function generateProfile(input, output) {
        var entries = [];
        for (var _i = 0, _a = Data.functions; _i < _a.length; _i++) {
            var func = _a[_i];
            var pts = selectProfilePoints(func.calls, input, output);
            var reg = findBestFit(pts);
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
    function makeCell(str, row, escape) {
        if (escape === void 0) { escape = true; }
        var elt = document.createElement("td");
        elt.innerHTML = escape ? escapeHtml(str) : str;
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
            func.classList.add("code");
            // 2. curve
            var fit = entry_1.fit;
            var fitCell = makeCell(isNaN(fit.equation[0]) ? "-" : fit.string, row, false);
            fitCell.dataset["sort"] = fit.sort;
            // 3. r^2
            var r2Cell = makeCell(isNaN(entry_1.fit.r2) ? "-" : entry_1.fit.r2.toFixed(2), row);
            r2Cell.classList.add("numeric");
            // 4. # calls
            var callCell = makeCell(entry_1.calls, row);
            callCell.classList.add("numeric");
            // 5. avg output
            var avgCell = makeCell(entry_1.output.toFixed(2), row);
            avgCell.classList.add("numeric");
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
    function renderSubTable(input, output, entry) {
        // aggregate by callsite
        var callSites = {};
        for (var _i = 0, _a = entry.points; _i < _a.length; _i++) {
            var pt = _a[_i];
            if (!callSites.hasOwnProperty(pt[2]))
                callSites[pt[2]] = [];
            callSites[pt[2]].push(pt);
        }
        var callSiteNames = Object.keys(callSites);
        callSiteNames.sort();
        // update the output column
        document.getElementById("suboutput-col").innerHTML = "Avg " + Profile.selected.output;
        // update the header
        for (var _b = 0, _c = document.querySelectorAll(".selected-function"); _b < _c.length; _b++) {
            var elt = _c[_b];
            elt.innerHTML = escapeHtml(entry.name.split(" ")[0]);
        }
        // remove existing rows
        for (var _d = 0, _e = document.querySelectorAll("table#subprofile tbody tr"); _d < _e.length; _d++) {
            var node = _e[_d];
            node.parentNode.removeChild(node);
        }
        var tbody = document.querySelectorAll("table#subprofile tbody")[0];
        // render row for each callsite
        for (var _f = 0, callSiteNames_1 = callSiteNames; _f < callSiteNames_1.length; _f++) {
            var callSite = callSiteNames_1[_f];
            var row = document.createElement("tr");
            var pts = callSites[callSite];
            // fit the data
            var reg = findBestFit(pts);
            var sum = pts.map(function (v) { return v[1]; }).reduce(function (a, b) { return a + b; }, 0);
            // 1. call site name
            makeCell(callSite, row);
            // 2. fit
            var fit = makeCell(isNaN(reg.equation[0]) ? "-" : reg.string, row, false);
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
        var callSites = {};
        for (var _i = 0, _a = Data.functions; _i < _a.length; _i++) {
            var func = _a[_i];
            if (!callSites.hasOwnProperty(func.name))
                callSites[func.name] = {};
            for (var _b = 0, _c = func["calls"]; _b < _c.length; _b++) {
                var fcall = _c[_b];
                if (!callSites[func.name].hasOwnProperty(fcall.location))
                    callSites[func.name][fcall.location] = [];
                callSites[func.name][fcall.location].push(fcall);
            }
        }
        var mergeCounts = [];
        var heapSizes = [];
        for (var func in callSites) {
            for (var loc in callSites[func]) {
                var myMergeCounts = [];
                var ioHeapSizes = [];
                for (var _d = 0, _e = callSites[func][loc]; _d < _e.length; _d++) {
                    var fcall = _e[_d];
                    if (fcall.metrics.hasOwnProperty("merge-count (excl.)")) {
                        myMergeCounts.push(fcall.metrics["merge-count (excl.)"]);
                    }
                    if (fcall.inputs.hasOwnProperty("heap-size")
                        && fcall.outputs.hasOwnProperty("heap-size")) {
                        ioHeapSizes.push([fcall.inputs["heap-size"], fcall.outputs["heap-size"]]);
                    }
                }
                if (myMergeCounts.length > 0) {
                    var mcSum = myMergeCounts.reduce(function (a, b) { return a + b; }, 0);
                    var mcAvg = mcSum / myMergeCounts.length;
                    mergeCounts.push({ func: func, loc: loc, mc: mcAvg });
                }
                var myMax = 0.0;
                for (var _f = 0, ioHeapSizes_1 = ioHeapSizes; _f < ioHeapSizes_1.length; _f++) {
                    var ioHeapSize = ioHeapSizes_1[_f];
                    if (ioHeapSize[0] > 0) {
                        var ratio = ioHeapSize[1] / ioHeapSize[0];
                        if (ratio > myMax) {
                            myMax = ratio;
                        }
                    }
                }
                heapSizes.push({ func: func, loc: loc, ratio: myMax });
            }
        }
        var highlights = [];
        if (heapSizes.length > 0) {
            heapSizes.sort(function (a, b) { return a.ratio < b.ratio ? 1 : (a.ratio > b.ratio ? -1 : 0); });
            var top5pct = Math.min(Math.ceil(heapSizes.length / 20), 4);
            for (var i = 0; i <= top5pct; i++) {
                var entry = heapSizes[i];
                if (entry.ratio >= 2) {
                    highlights.push({ func: entry.func, loc: entry.loc, reason: "heap size blowup (" + entry.ratio.toFixed(2) + ")" });
                }
            }
        }
        if (mergeCounts.length > 0) {
            mergeCounts.sort(function (a, b) { return a.mc < b.mc ? 1 : (a.mc > b.mc ? -1 : 0); });
            var top5pct = Math.min(Math.ceil(mergeCounts.length / 20), 4);
            for (var i = 0; i <= top5pct; i++) {
                var entry = mergeCounts[i];
                if (entry.mc > 10) {
                    highlights.push({ func: entry.func, loc: entry.loc, reason: "high merge count (" + entry.mc.toFixed(2) + ")" });
                }
            }
        }
        var tbody = document.querySelector("#important tbody");
        for (var _g = 0, highlights_1 = highlights; _g < highlights_1.length; _g++) {
            var hi = highlights_1[_g];
            var row = document.createElement("tr");
            // 1. name
            var _h = hi.func.split(" "), name_1 = _h[0], rest = _h.slice(1);
            var body = "<code>" + escapeHtml(name_1) + "</code> (called at " + escapeHtml(hi.loc) + ")";
            var func = makeCell(body, row, false);
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
        for (var _i = 0, _a = document.querySelectorAll('.selected'); _i < _a.length; _i++) {
            var elt = _a[_i];
            elt.classList.remove('selected');
        }
        if (entry != null) {
            entry.row.classList.add('selected');
            renderGraph(Profile.selected.input, Profile.selected.output, entry);
            renderSubTable(Profile.selected.input, Profile.selected.output, entry);
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
})(profile || (profile = {}));
document.addEventListener("DOMContentLoaded", profile.init);
//# sourceMappingURL=profile.js.map