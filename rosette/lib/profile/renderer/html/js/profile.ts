/// <reference path="data.ts" />

// convince TS that document.querySelectorAll can be an array
interface NodeList extends Array<Node> { }

namespace profile {
    
    declare var vg;            // vega
    declare var regression;    // regression.js
    declare var Tablesort;     // tablesort.js
    
    // global state
    export let Profile = {
        // populated by initData
        inputs: [],
        outputs: [],
        metrics: [],
        graph: null,
        calls: [],
        columns: [],
        sorter: null
    };
    
    export function init() {
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
    
    function initData() {
        Profile.graph = eventsToGraph(Data["events"]);
        // walk the graph and build inclusive metrics
        let worklist = [Profile.graph];
        let columns = {};
        while (worklist.length > 0) {
            let node = worklist.pop();
            node["delta"] = {};
            for (let m in node["finish"]) {
                if (node["start"].hasOwnProperty(m)) {
                    node["delta"][m] = node["finish"][m] - node["start"][m];
                    columns[m] = true;
                }
            }
            Profile.calls.push(node);
            worklist.push(...node["children"].reverse());  // traverse depth-first in start-time order
        }
        Profile.columns = Object.keys(columns).sort();
        // walk the graph and build exclusive metrics
        for (let call of Profile.calls) {
            for (let m of Profile.columns) {
                let key = m + " (excl)";
                let childSum = call["children"].reduce((a, x) => a + x["delta"][m], 0);
                call["delta"][key] = call["delta"][m] - childSum;
                columns[key] = true;
            }
        }
        Profile.columns = Object.keys(columns).sort();
    }

    function renderTable() {
        // create column headers
        let table = document.getElementById("profile");
        let head = table.querySelector("thead tr");
        for (let c of Profile.columns) {
            let th = document.createElement("th");
            th.innerHTML = c;
            th.dataset["sortMethod"] = "number";
            head.insertAdjacentElement("beforeend", th);
        }

        // create table rows when time is sufficiently large
        let dt = 0.001 * Profile.graph["metrics"]["time"];
        let minTime = Profile.graph["start"]["time"];
        let body = table.querySelector("tbody");
        for (let node of Profile.calls) {
            if (node["metrics"]["time"] < dt)
                continue;
            let tr = document.createElement("tr");
            makeCell(node["function"], tr).className = "code";
            makeCell(formatNum(node["start"]["time"] - minTime), tr);
            makeCell(formatNum(node["finish"]["time"] - minTime), tr);
            for (let m of Profile.columns) {
                let val = node["delta"].hasOwnProperty(m) ? node["delta"][m] : 0;
                makeCell(formatNum(val), tr);
            }
            body.insertAdjacentElement("beforeend", tr);
        }

        Profile.sorter = new Tablesort(table, { descending: true });
    }

    function formatNum(v, places=2) {
        return v % 1 == 0 ? v : v.toFixed(places);
    }
    
    function makeCell(str, row, escape = true) {
        let elt = document.createElement("td");
        elt.innerHTML = escape ? escapeHtml(str) : str;
        row.insertAdjacentElement('beforeend', elt);
        return elt;
    }

}

document.addEventListener("DOMContentLoaded", dataOnload(profile.init, profile.init));