var init_desc;

var margin = {top: 30, right: 20, bottom: 30, left: 20},
    width = 960,
    barHeight = 20,
    barWidth = (width - margin.left - margin.right) * 0.2;

var i = 0,
    duration = 400,
    root;

var diagonal = d3.linkHorizontal()
    .x(function (d) {
        return d.y;
    })
    .y(function (d) {
        return d.x;
    });

var svg_tree = d3.select("#d3_graphs").append("svg")
    .attr("width", width) // + margin.left + margin.right)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

function reload_tax_tree(data) {
    root = d3.hierarchy(data);
    root.x0 = 0;
    root.y0 = 0;
    // open the tree collapsed
    desc = root.descendants();
    init_desc = root.descendants();
    var i = 0;
    for (i = 0; i < desc.length; i++) {
        desc[i]._children = desc[i].children;
        desc[i].children = null;
        desc[i].selected = 0;
        desc[i].found = 0;
    }
    update(root);
}

reload_tax_tree({"name": "Organisms", "children": [], "taxLevel": "organisms", "types": 1,
             "ratio": 1.0, "size_tree": 1, "redList": 1 });


var min_width = 5;

function update(source) {

    // Compute the flattened node list.
    var nodes = root.descendants();

    var height = Math.max(500, nodes.length * barHeight + margin.top + margin.bottom);

    d3.select("#d3_graphs").transition()
        .duration(duration)
        .attr("height", height);

    d3.select(self.frameElement).transition()
        .duration(duration)
        .style("height", height + "px");

    // Compute the "layout". TODO https://github.com/d3/d3-hierarchy/issues/67
    var index = -1;
    root.eachBefore(function (n) {
        n.x = ++index * barHeight;
        n.y = n.depth * 20;
    });

    // Update the nodes…
    var node = svg_tree.selectAll(".node")
        .data(nodes, function (d) {
            return d.id || (d.id = ++i);
        });

    var nodeEnter = node.enter().append("g")
        .attr("class", "node")
        .attr("transform", function (d) {
            return "translate(" + source.y0 + "," + source.x0 + ")";
        })
        .style("opacity", 0);

    // Enter any new nodes at the parent's previous position.
    nodeEnter.append("rect")
        .attr("y", -barHeight / 2)
        .attr("height", barHeight)
        .attr("width", (function (d) {
            var curr_width = barWidth * d.data.ratio;
            if (curr_width < min_width) {
                return min_width;
            }
            return curr_width;
        }))
        .style("fill", color)
        .style("opacity", 0.5)
        .on("click", clickTree);

    nodeEnter.append("text")
        .attr("dy", 3.5)
        .attr("dx", 5.5)
        .text(function (d) {
            return (d.data.name)
        });

    nodeEnter.on("mouseover", handleMouseOver)
        .on("mouseout", handleMouseOut)
    .on("mousemove", handleMouseMove );

    // text is taxonomy level, value is the actual taxonomy name
    nodeEnter.append("circle")
        .attr("cy", 0)
        .attr("cx", -5)
        //.text(function (d) {
        //    return d.data.taxLevel;
        //})
        .attr("r", 3)
        //.attr("value", function(d) {
        //    return d.data.name;
        //})
        .attr("fill", "white")
        .style("stroke", function(d) {
            if (d.data.redList) {
                return "red";
            }
            return "black";
        })
        .on("click", function (d) {
            var elem = d3.select(this);
            // if unselect
            if (d.selected) {
                elem.attr("fill", "white");
                d.selected = 0;
            } else {
                elem.attr("fill", "black");
                // make it equal to d.data.id
                d.selected = 1;
            }

            //console.log(elem.text());
            //console.log(elem.attr("value"));
            // var taxon_name = d3.select(this).text();
        });

    // Transition nodes to their new position.

    function node_blink() {
        nodeEnter.transition()
        .duration(duration)
        .attr("transform", function (d) {
            return "translate(" + d.y + "," + d.x + ")";
        })
        .style("opacity", function(d) {
            if (d.found == 1) {
                return 0.1;
            }
            else {
                return 1;
            }
        })
        .transition()
        .duration(duration*2)
        .style("opacity", 1)
        .on("end", node_blink);
    }

    node_blink();

    node.transition()
        .duration(duration)
        .attr("transform", function (d) {
            return "translate(" + d.y + "," + d.x + ")";
        })
        .style("opacity", 1)
        .select("rect")
        .style("fill", color);

    // Transition exiting nodes to the parent's new position.
    node.exit().transition()
        .duration(duration)
        .attr("transform", function (d) {
            return "translate(" + source.y + "," + source.x + ")";
        })
        .style("opacity", 0)
        .remove();

    // Update the links…
    var link = svg_tree.selectAll(".link")
        .data(root.links(), function (d) {
            return d.target.id;
        });

    // Enter any new links at the parent's previous position.
    link.enter().insert("path", "g")
        .attr("class", "link")
        .attr("d", function (d) {
            var o = {x: source.x0, y: source.y0};
            return diagonal({source: o, target: o});
        })
        .transition()
        .duration(duration)
        .attr("d", diagonal);


    // Transition links to their new position.
    link.transition()
        .duration(duration)
        .attr("d", diagonal);

    // Transition exiting nodes to the parent's new position.
    link.exit().transition()
        .duration(duration)
        .attr("d", function (d) {
            var o = {x: source.x, y: source.y};
            return diagonal({source: o, target: o});
        })
        .remove();

    // Stash the old positions for transition.
    root.each(function (d) {
        d.x0 = d.x;
        d.y0 = d.y;
    });
}

// Toggle children on click.
function clickTree(d) {
    curr_children = d.children;
    if (d.children) {
        curr_desc = d.descendants();
        for (i = 1; i < curr_desc.length; i++) {
            curr_desc[i].selected = 0;
        }
        d._children = d.children;
        d.children = null;
    } else {
        d.children = d._children;
        d._children = null;
    }
    update(d);
}

function color(d) {
    // var nextChildren = d.children;
    // if (!nextChildren) {
    //     nextChildren = d._children;
    //     if (!nextChildren) {
    //         return "#fd8d3c"
    //     }
    // }
    var height = d.height;
    switch (height) {
        case 6:
            return "#8240bd"
        case 5:
            return "#bd47a0";
        case 4:
            return "#35b9bd";
        case 3:
            return "#0932bd";
        case 2:
            return "#bd1e17";
        case 1:
            return "#3fbd20";
        case 0:
            return "#bdb7bd";
        default:
            return "#ffa500";
    }

    // return d._children ? "#3182bd" : d.children ? "#c6dbef" : "#fd8d3c";
}

function getRandomColor() {
    var letters = '0123456789ABCDEF';
    var color = '#';
    for (var i = 0; i < 6; i++) {
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
}

function searchTaxon() {
    term = $("#taxon-search-field").val();
    results = [];
    for (i = 0; i < init_desc.length; i++) {
        if (term.toLowerCase() == init_desc[i].data.name.toLowerCase()) {
            init_desc[i].found = 1;
            results.push(init_desc[i])
        }
        else {
            init_desc[i].found = 0;
        }

    }

    if (results.length == 0) {
        alert("Search returned no results")
    }
    else {
        results.forEach( function(result) {
            while (result.parent) {
                if (result.parent._children) {
                    // result.parent.found = 1;
                    clickTree(result.parent);
                }
                result = result.parent;
            }
        })
    }

}

function searchTaxonEnter() {
    if(event.key === 'Enter') {
        searchTaxon();
    }
}

taxon_tree_stats = document.getElementById("taxon-stats");

function handleMouseOver(d,i) {
    curr_elem = d3.select(this);


    curr_elem.append("rect")
        // .attr("x",(function (d) {
        //     var curr_width = barWidth * d.data.ratio;
        //     if (curr_width < min_width) {
        //         return min_width;
        //     }
        //     return curr_width/2;
        // }))
        .attr("x", d3.mouse(this)[0]+20)
        .attr("y", -barHeight / 2)
        .attr("height", barHeight)
        .attr("width", 100)
        .attr("rx", 10)
        .style("fill", "black")
        .attr("id", "r" + d.x + "-" + d.y + "-" + i);


    curr_elem.append("text")
        .attr("dy", 3.5)
        .attr("x", d3.mouse(this)[0]+25)
        // .attr("dx", (function (d) {
        //     var curr_width = barWidth * d.data.ratio;
        //     if (curr_width < min_width) {
        //         return min_width;
        //     }
        //     return curr_width/2;
        // }))
        .attr("id", "t" + d.x + "-" + d.y + "-" + i)
        .style('fill', 'white')
        .text(function (d) {
            return (" species:" + d.data.size_tree + " types:" + d.data.types)
        });

}

function handleMouseOut(d, i) {
            // Use D3 to select element, change color back to normal
            // Select text by id and then remove
    d3.select("#t" + d.x + "-" + d.y + "-" + i).remove();  // Remove text location
    d3.select("#r" + d.x + "-" + d.y + "-" + i).remove();
}

function handleMouseMove(d, i) {
            // Use D3 to select element, change color back to normal
            // Select text by id and then remove
    d3.select("#t" + d.x + "-" + d.y + "-" + i).attr("x", d3.mouse(this)[0] +25);  // Remove text location
    d3.select("#r" + d.x + "-" + d.y + "-" + i).attr("x", d3.mouse(this)[0] +20);
}

