/// <reference path="data.ts" />

// convince TS that document.querySelectorAll can be an array
interface NodeList extends Array<Node> {}

namespace timeline {

declare var vg;            // vega

export function init() {
    // Initialize the profile data
    initData();

    // update the profile source info
    document.getElementById("name").innerHTML = "Timeline: " + Data.metadata.name;
    document.getElementById("source").innerHTML = Data.metadata.source;
    document.getElementById("form").innerHTML = Data.metadata.form;
    document.getElementById("time").innerHTML = Data.metadata.time;
    document.title = "Timeline: " + Data.metadata.name;

    // Render the timeline
    renderTimeline("terms");
}

function renderTimeline(metric: string) {
    let timeline = document.getElementById("timeline");

    let points = [];
    let startTime = Infinity;
    for (let func of Data.functions) {
        for (let fcall of func["calls"]) {
            for (let key of ["start", "finish"]) {
                if (!fcall.hasOwnProperty(key))
                    continue;
                let data = fcall[key];
                if (!data.hasOwnProperty("time") || !data.hasOwnProperty(metric))
                    continue;
                points.push({"time": data["time"], "value": data[metric]});
                if (data["time"] < startTime)
                    startTime = data["time"];
            }
        }
    }
    console.log(startTime);
    for (var i = 0; i < points.length; i++) {
        points[i]["time"] = points[i]["time"] - startTime;
    }

    // render the vega spec
    let spec = {
        "data": {
            "values": points
        },
        "mark": "line",
        "width": 400,
        "height": 400,
        "encoding": {
            "x": {
                "field": "time",
                "type": "quantitative",
                "axis": {
                    "title": "time"
                }
            },
            "y": {
                "field": "value",
                "type": "quantitative",
                "axis": {
                    "title": metric
                }
            }
        }
    };
    vg.embed(timeline, {mode: "vega-lite", spec: spec}, function(err, res) {});

    console.log("timeline:", points);
}

} // /namespace

document.addEventListener("DOMContentLoaded", timeline.init);