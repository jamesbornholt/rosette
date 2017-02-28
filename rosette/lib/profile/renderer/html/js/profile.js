/// <reference path="data.ts" />
var profile;
(function (profile) {
    // global state
    profile.Profile = {
        // populated by initData
        inputs: [],
        outputs: [],
        metrics: [],
        graph: null,
        calls: [],
        columns: [],
        sorter: null
    };
    function init() {
        // Initialize the profile data
        initData();
        // update the profile source info
        document.getElementById("name").innerHTML = "Profile: " + Data.metadata.name;
        document.getElementById("source").innerHTML = Data.metadata.source;
        document.getElementById("form").innerHTML = Data.metadata.form;
        document.getElementById("time").innerHTML = Data.metadata.time;
        document.title = "Profile: " + Data.metadata.name;
        // render the initial table
        renderTable();
    }
    profile.init = init;
    function initData() {
        profile.Profile.graph = eventsToGraph(Data["events"]);
        // walk the graph and build inclusive, exclusive metrics
        var worklist = [profile.Profile.graph];
        var columns = {};
        while (worklist.length > 0) {
            var node = worklist.pop();
            profile.Profile.calls.push(node);
            for (var m in node["metrics"]) {
                columns[m] = true;
            }
            worklist.push.apply(worklist, node["children"].reverse()); // traverse depth-first in start-time order
        }
        profile.Profile.columns = Object.keys(columns).sort();
    }
    function renderTable() {
        // create column headers
        var table = document.getElementById("profile");
        var head = table.querySelector("thead tr");
        for (var _i = 0, _a = profile.Profile.columns; _i < _a.length; _i++) {
            var c = _a[_i];
            var th = document.createElement("th");
            th.innerHTML = c;
            th.dataset["sortMethod"] = "number";
            head.insertAdjacentElement("beforeend", th);
        }
        // create table rows when time is sufficiently large
        var dt = 0.001 * profile.Profile.graph["metrics"]["time"];
        var minTime = profile.Profile.graph["start"]["time"];
        var body = table.querySelector("tbody");
        for (var _b = 0, _c = profile.Profile.calls; _b < _c.length; _b++) {
            var node = _c[_b];
            if (node["metrics"]["time"] < dt)
                continue;
            var tr = document.createElement("tr");
            makeCell(node["function"], tr).className = "code";
            makeCell(formatNum(node["start"]["time"] - minTime), tr);
            makeCell(formatNum(node["finish"]["time"] - minTime), tr);
            for (var _d = 0, _e = profile.Profile.columns; _d < _e.length; _d++) {
                var m = _e[_d];
                var val = node["metrics"].hasOwnProperty(m) ? node["metrics"][m] : 0;
                makeCell(formatNum(val), tr);
            }
            body.insertAdjacentElement("beforeend", tr);
        }
        profile.Profile.sorter = new Tablesort(table, { descending: true });
    }
    function formatNum(v, places) {
        if (places === void 0) { places = 2; }
        return v % 1 == 0 ? v : v.toFixed(places);
    }
    function makeCell(str, row, escape) {
        if (escape === void 0) { escape = true; }
        var elt = document.createElement("td");
        elt.innerHTML = escape ? escapeHtml(str) : str;
        row.insertAdjacentElement('beforeend', elt);
        return elt;
    }
})(profile || (profile = {}));
document.addEventListener("DOMContentLoaded", dataOnload(profile.init, profile.init));
//# sourceMappingURL=profile.js.map