/// <reference path="data.ts" />
var timeline;
(function (timeline_1) {
    function init() {
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
    timeline_1.init = init;
    function renderTimeline(metric) {
        var timeline = document.getElementById("timeline");
        var points = [];
        var startTime = Infinity;
        for (var _i = 0, _a = Data.functions; _i < _a.length; _i++) {
            var func = _a[_i];
            for (var _b = 0, _c = func["calls"]; _b < _c.length; _b++) {
                var fcall = _c[_b];
                for (var _d = 0, _e = ["start", "finish"]; _d < _e.length; _d++) {
                    var key = _e[_d];
                    if (!fcall.hasOwnProperty(key))
                        continue;
                    var data = fcall[key];
                    if (!data.hasOwnProperty("time") || !data.hasOwnProperty(metric))
                        continue;
                    points.push({ "time": data["time"], "value": data[metric] });
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
        var spec = {
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
        vg.embed(timeline, { mode: "vega-lite", spec: spec }, function (err, res) { });
        console.log("timeline:", points);
    }
})(timeline || (timeline = {})); // /namespace
document.addEventListener("DOMContentLoaded", timeline.init);
//# sourceMappingURL=timeline.js.map