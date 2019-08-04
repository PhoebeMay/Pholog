
var title = d3.select("body").append("h1")
    .attr("align", "center")
    .html(" Anonymous ");

var mode_widget_vert = 60;

d3.select("body").append("div")
    .attr("align", "center")
    .text("Mouse over the graph to show where values were allocated.  " +
          "Values allocated from non-OCaml code have their mouse-over popup " +
          "text in green.  Click a portion of the graph to move up the stack.")

var mode_widget =
  d3.select("body")
    .append("ul")
    .attr("class", "mode");

var margin = {top: 100, right: 200, bottom: 100, left: 200};

var width = 1280 - margin.left - margin.right;

var height = 640 - margin.top - margin.bottom;

var popup_padding = 4;

var x = d3.scale.linear().range([0, width]);

var y = d3.scale.linear().range([height, 0]);

var color1 = d3.scale.category20();
var color2 = d3.scale.category20c();

var fmt = d3.format('.3');

var xAxis = d3.svg.axis().scale(x).orient("bottom");

xAxis.tickFormat(function (secs) {
  return fmt(secs) + 's';
});

var rt2 = Math.sqrt(2);
var ln2 = Math.log(2);

var byteTickValues = function(start, stop, count) {

  var step0 = Math.abs(stop - start) / Math.max(0, count),
      step1 = Math.pow(2, Math.floor(Math.log(step0) / ln2)),
      error = step0 / step1;

  if (error >= rt2) step1 *= 2;

  var step = stop < start ? -step1 : step1;

  return d3.range(
    Math.ceil(start / step) * step,
    Math.floor(stop / step) * step + step / 2, // inclusive
    step
  );
};

var kb = 1024
var mb = kb * 1024
var gb = mb * 1024

var byteTickFormat = function (bytes) {
  return fmt(bytes) + 'B';
}

var kilobyteTickFormat = function (bytes) {
    return fmt(bytes / kb) + 'kB';
}

var megabyteTickFormat = function (bytes) {
    return fmt(bytes / mb) + 'MB';
}

var gigabyteTickFormat = function (bytes) {
    return fmt(bytes / gb) + 'GB';
}

var yAxis = d3.svg.axis().scale(y).orient("left");

var area = d3.svg.area()
    .x(function(d) { return x(d.time); })
    .y0(function(d) { return y(d.y0); })
    .y1(function(d) { return y(d.y0 + d.y); });

var stack = d3.layout.stack()
    .values(function(d) { return d.values; });

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

svg.append("g")
  .attr("transform", "translate(-75, " + height / 2 + ")")
  .append("text")
  .attr("text-anchor", "middle")
  .attr("transform", "rotate(-90)")
  .text("Number of words or blocks")

svg.append("g")
  .attr("transform", "translate(" + width / 2 + ", " + (height + 40) + ")")
  .append("text")
  .attr("text-anchor", "middle")
  .text("Time since program start")

var body =
  svg.append("g")
    .on("mouseover", function() {
      popup.transition().duration(500).style("opacity", 1);
    })
    .on("mouseout", function() {
      popup.transition().duration(10).style("opacity", 0);
    })
    .on("mousemove", function(d) {
      var x = d3.event.pageX - margin.left + 5;
      var y = d3.event.pageY - margin.top - mode_widget_vert + 5;
      popup.attr("transform", "translate(" + x + "," + y + ")");
    });

var popup =
  svg.append("g")
    .attr("id", "popup")
    .attr("pointer-events", "none")
    .style("opacity", 0);

popup.append("rect")
  .attr("fill", "#000000")
  .attr("stroke", "#969696")
  .attr("rx", "5")
  .attr("ry", "5")
  .style("opacity", .6);

popup.append("text")
  .attr("fill", "#FFFFFF");

svg.append("g")
  .attr("class", "axis")
  .attr("id","xaxis")
  .attr("transform", "translate(0," + height + ")");

svg.append("g")
  .attr("class", "axis")
  .attr("id","yaxis");

d3.select("body").append("h2")
    .html("Backtrace (oldest frame first):")

var frame_widget = d3.select("body").append("ul");

function graph(input, dispatch) {

  var modes =
    mode_widget.selectAll("li")
      .data(input.modes);

  modes.enter().append("li");

  modes
    .text(function (d) { return d.display })
    .attr("class", function (d) {
      if (d.selected) { return "mode-sel" }
      else { return "mode" }
    })
    .on("click", function (d) {
      if (!d.selected) { dispatch.select(d.path); }
    });

  modes.exit().remove();

  color1.domain(input.layers.map(function (layer)
     { return layer.index }));
  color2.domain(input.layers.map(function (layer)
     { return location.index }).reverse());

  var layers = stack(input.layers);

  var max_time = input.max_time;
  var max_y = input.max_y;

  x.domain([0, max_time]);
  y.domain([0, max_y]);

  if(input.bytes) {
    yAxis.tickValues(byteTickValues(0, max_y, 10));
    if (max_y < kb) {
      yAxis.tickFormat(byteTickFormat);
    } else if (max_y < mb) {
      yAxis.tickFormat(kilobyteTickFormat);
    } else if (max_y < gb) {
      yAxis.tickFormat(megabyteTickFormat);
    } else {
      yAxis.tickFormat(gigabyteTickFormat);
    }
  } else {
    yAxis.tickValues(null);
    yAxis.tickFormat(null);
    yAxis.ticks(10, ",s");
  }

  svg.select("#xaxis").call(xAxis);
  svg.select("#yaxis").call(yAxis);

  var layer =
    body.selectAll(".layer")
      .data(layers);

  layer.enter()
    .append("g")
    .attr("class", "layer")
    .append("path", "#popup")
    .attr("class", "area");

  layer.select("path")
    .attr("d", function(d) { return area(d.values); })
    .on("mouseenter", function(d) {
      d3.select(this).attr("stroke", "#000000");
      var x = d3.event.pageX - margin.left + 5;
      var y = d3.event.pageY - margin.top - mode_widget_vert + 5;
      popup.attr("transform", "translate(" + x + "," + y + ")");
      var text = popup.select("text");
      text.text(d.display);
      if(d.foreign) {
        text.attr("fill", "#66FF00");
      } else {
        text.attr("fill", "#FFFFFF");
      }
      var box = text.node().getBBox();
      popup.select("rect")
        .attr("x", box.x - popup_padding)
        .attr("y", box.y - popup_padding)
        .attr("width", box.width + popup_padding*2)
        .attr("height", box.height + popup_padding*2);
    })
    .on("mouseleave", function(d) {
      d3.select(this).attr("stroke", "none");
    })
    .on("click", function (d) { dispatch.select(d.path); })
    .attr("fill", function(d) {
       if(input.depth % 2 == 0) {
         return color1(d.index);
       } else {
         return color2(d.index);
       }
     });

  layer.exit().remove();

  var frame =
    frame_widget.selectAll("li")
      .data(input.frames);

  frame.enter()
    .append("li")
    .style("cursor","pointer")
    .style("color","blue")
    .style("font","12px sans-serif")
    .style("text-decoration","underline");

  frame.text(function (d) { return d.display;});
  frame.on("click", function (d) { return dispatch.frame(d.path); });

  frame.exit().remove();

}

var dispatch = d3.dispatch("select", "frame");

function fetch(path) {
  var xmlhttp = new XMLHttpRequest();
  xmlhttp.onload = function () {
    if(xmlhttp.status == 200) {
      var state = JSON.parse(xmlhttp.responseText);
      graph(state, dispatch);
    }
  };
  xmlhttp.open("GET", path, true);
  xmlhttp.send();
}

fetch("initial.json");
dispatch.on("select", function (path) { if(path != null) fetch(path); });
dispatch.on("frame", function (path) { fetch(path); });
