var color_legend = d3.scaleOrdinal()
    .domain(['organisms', 'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species',])
    .range(["#ffa500", "#8240bd", "#bd47a0", "#35b9bd", "#0932bd", "#bd1e17", "#3fbd20", "#bdb7bd"]);

var legendRectSize = 20;
var legendSpacing = 12;


var legend = d3.select("#labels")
    .append("svg")
    .append("g")
    .selectAll("g")
    .data(color_legend.domain())
    .enter()
    .append('g')
    .attr('class', 'legend')
    .attr('transform', function (d, i) {
        var height = legendRectSize;
        var x = 0;
        var y = i * height;
        return 'translate(' + x + ',' + y + ')';
    });

legend.append('rect')
    .attr('width', legendRectSize)
    .attr('height', legendRectSize)
    .style('fill', color_legend)
    .style('fill-opacity', 0.5);

legend.append('text')
    .attr('x', legendRectSize + legendSpacing - 5)
    .attr('y', 5 + legendRectSize - legendSpacing)
    .text(function (d) {
        return d;
    });