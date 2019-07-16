var bar_chart_ref;
var sunburst_ref;

L.TopoJSON = L.GeoJSON.extend({
    addData: function (data) {
        var geojson, key;
        if (data.type === "Topology") {
            for (key in data.objects) {
                if (data.objects.hasOwnProperty(key)) {
                    geojson = topojson.feature(data, data.objects[key]);
                    L.GeoJSON.prototype.addData.call(this, geojson);
                }
            }

            return this;
        }

        L.GeoJSON.prototype.addData.call(this, data);

        return this;
    }
});

L.topoJson = function (data, options) {
    return new L.TopoJSON(data, options);
};

function countKeys(obj) {
    return Object.keys(obj).length;
}


var highlight = {
    'color': '#ffffff',
    'weight': 2,
    'opacity': 1,
    'fillColor': '#ffffff',
    'fillOpacity': 0.5
};

var remove_highlight = {
    'color': "#d2d2d2",
    'weight': 1,
    'fillColor': "#8d8c91",
    'fillOpacity': 1
};

var municipality_style = {
    'color': 'rgb(94,94,94)',
    'weight': 2,
    'opacity': 1,
    'fillColor': '#ffffff',
    'fillOpacity': 0.5
};


var typeEco = {
    "ME": "#F012BE",
    "SE": "#85144b",
    "XX": "rgb(39,39,48)",
    "YS": "#c9c731"
};

var typeSE = {
    "OF": "#2ECC40",
    "MF": "#3D9970",
    "WD": "#653f2e",
    "RI": "#00236e",
    "IT": "#eeaf8c",
    "WN": "#0074D9",
    "HB": "#98Fb98",
    "SV": "#857e4e",
    "ES": "#7FDBFF",
    "FW": "#39CCCC",
    "AP": "#AAAAAA",

};

// initialize the map


var cartodbAttribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="http://cartodb.com/attributions">CartoDB</a>';

// 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
grayScaleBaseMap = L.tileLayer('http://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png',
    {
        edgeBufferTiles: 1,
        attribution: cartodbAttribution,
        maxZoom: 17,
        minZoom: 2
    });


streetsBaseMap = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    {
        edgeBufferTiles: 1,
        attribution: cartodbAttribution,
        maxZoom: 19,
        minZoom: 2
    });

var baseLayers = {
    "Grayscale": grayScaleBaseMap,
    "Streets": streetsBaseMap
};

sei_layer = L.geoJson(false, {
        style: function (feature) {
            sstyle = remove_highlight;
            if (feature.properties.SE_ME_1 != "SE") {
                sstyle.fillColor = typeEco[feature.properties.SE_ME_1];
            } else {
                sstyle.fillColor = typeSE[feature.properties.SECl_1];
            }

            return sstyle;
        },
        onEachFeature: function (feature, layer) {
            var popUpInfo = feature.properties.Comp1Lgnd_;
            if (feature.properties.Comp2Lgnd_) {
                popUpInfo += "<br/>" + feature.properties.Comp2Lgnd_;
                if (feature.properties.Comp3Lgnd_) {
                    popUpInfo += "<br/>" + feature.properties.Comp3Lgnd_;
                }
            }
            popUpInfo += "</br> Quality: " + feature.properties.QualityNo_ + "/5.0";
            if (feature.properties.Location) {
                popUpInfo += "</br> Location: " + feature.properties.Location;
            }
            // var popUp = layer.bindPopup(popUpInfo);
            // popUp.on('popupclose', function() {
            //     // layer.setStyle(remove_highlight);
            // });
            layer.on('click', function () {
                layer.bringToFront();
                if (layer.selected) {
                    sstyle = remove_highlight;
                    if (feature.properties.SE_ME_1 != "SE") {
                        sstyle.fillColor = typeEco[feature.properties.SE_ME_1];
                    } else {
                        sstyle.fillColor = typeSE[feature.properties.SECl_1];
                    }
                    layer.setStyle(sstyle);
                    layer.selected = 0;
                    layer.unbindPopup();
                } else {
                    sstyle = highlight;
                    // if (feature.properties.SE_ME_1 != "SE") {
                    //     sstyle.fillColor = typeEco[feature.properties.SE_ME_1];
                    // }
                    // else {
                    //     sstyle.fillColor = typeSE[feature.properties.SECl_1];
                    // }
                    layer.setStyle(sstyle);
                    layer.selected = 1;
                    layer.bindPopup(popUpInfo).openPopup();
                }
            })
        }
    });

mun_layer = L.geoJson(false, {
        onEachFeature: function (feature, layer) {
            layer.bindPopup(feature.properties.FullName);

            layer.on('click', function () {
                // change to newID
                showSummary(feature.properties.newID, null);
            });
            layer.setStyle(municipality_style);
        }

    });


var overlays = {Municipalities: mun_layer, SEI: sei_layer};
var num_overlays = 2;

var map = L.map('map', {
    center: [49.263710, -123.259378],
    zoom: 10,
    layers: [streetsBaseMap, grayScaleBaseMap]
});

// map.on('moveend', function() {
//      console.log(map.getBounds());
// });

L.control.layers(baseLayers, overlays, {collapsed: false}).addTo(map);

grayScaleBaseMap.addTo(map);

map.on('overlayadd', function(l) {
    if (l.layer == mun_layer) {
        json_path = "Municipalities.geojson";
    }
    else if (l.layer == sei_layer) {
        json_path = "SEI.geojson";
    }

    if (l.layer.toGeoJSON().features.length == 0) {
        $.getJSON(static_path + json_path, function (data) {
            l.layer.addData(data);
        });
    }

});

function openWiki(callback, speciesName) {
    var title = callback.query.search[0].title;

    var url = title.replace(/ /g, "_");

    var speciesId = speciesName.replace(/\s+/g, '_');

    var wikiButton = document.getElementById(speciesId + "_wiki");
    wikiButton.onclick = function () {
        window.open("https://en.wikipedia.org/wiki/" + url, "_blank");
    };
    wikiButton.value = "Wikipedia";
}


function loadImg(speciesName) { //AJAX request
    $.ajax({
        url: "https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=" + speciesName + "&prop=info&inprop=url&utf8=&format=json",
        dataType: "jsonp",
        success: function (response) {
            // console.log(response.query);
            if (response.query.searchinfo.totalhits === 0) {
                alert("Couldn't find wiki");
            } else {
                wikiImg(response.query.search[0].title, speciesName);
                openWiki(response, speciesName);
            }
        },
        error: function () {
            alert("Error retrieving search results, please refresh the page");
        }
    });
}


function wikiImg(pageTitle, speciesName) { //AJAX request
    $.ajax({
        url: "https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&sites=enwiki&props=claims&titles=" + pageTitle,
        dataType: "jsonp",
        success: function (response) {
            // console.log(response.query);
            // if (response.query.searchinfo.totalhits === 0) {
            // 	showError(keyword);
            // }

            // else {
            imgResults(response, speciesName);
            // }
        },
        error: function () {
            alert("Error retrieving search results, please refresh the page");
        }
    });
}

function imgResults(callback, speciesName) {

    var img_name = callback.entities;
    var key_entity = Object.keys(img_name)[0];
    img_name = callback.entities[key_entity].claims.P18[0].mainsnak.datavalue.value;

    img_name = img_name.replace(/\s+/g, '_');
    // console.log(img_name);
    var img_name_hash = md5(img_name);
    var img_url = 'https://upload.wikimedia.org/wikipedia/commons/' + img_name_hash[0] +
        '/' + img_name_hash.substr(0, 2) + '/' + img_name;
    // console.log(img_url);
    var speciesId = speciesName.replace(/\s+/g, '_');

    var img = new Image(),
        url = img_url,
        container = document.getElementById(speciesId + "_thumb");

    img.onload = function () {
        container.appendChild(img);
    };
    img.src = url;
    // return img_url;
}

var dotIcon = L.icon({
    iconUrl: 'blue_dot.png',
    iconSize: [10, 10]
});



var points_layer_options = {
    pointToLayer: function (feature, latlng) {
        var marker = L.marker(latlng, {icon: dotIcon});
        if (feature.properties) {
            var redList = feature.properties.redList;
            var markerPopUp = '<div class="popUpfeature">' + feature.properties.species + '<br/> <b>'
                + feature.properties.common + '</b>';
            if (redList) {
                markerPopUp += '<b class="redListFont">' + '<br/>' + feature.properties.redList + '</b>';
            }

            markerPopUp += '<br/>';

            var id_species = feature.properties.species;
            if (id_species) {
                id_species = id_species.replace(/\s+/g, '_');
            }

            // markerPopUp += '<input type="button" value="wikiImg" '+  ' onClick="loadImg(\'' + feature.properties.species + '\')" />';

            // markerPopUp += '<input type="button" value="Wiki" '+ ' onClick="goToWiki(\'' + feature.properties.species + '\')" />';

            markerPopUp += '<input type="button" class="wikiButton" value="Loading" id=' + (id_species + "_wiki") + '>';

            markerPopUp += '<div class="thumbnail"  id=' + (id_species + "_thumb") + '> </div>';

            markerPopUp += '</div>';

            marker.bindPopup(markerPopUp);

            marker.on('click', function () {
                loadImg(feature.properties.species);
            });
        }
        // var marker = L.marker(latlng, {title: ""});
        return marker;
    }
};

var clusters = L.markerClusterGroup(
            {
                iconCreateFunction: function (cluster) {
                    var i = 0;
                    var childrenMarkers = cluster.getAllChildMarkers();
                    var endangered = 0;
                    for (i = 0; i < childrenMarkers.length; i++) {
                        if (childrenMarkers[i].feature.properties.redList) {
                            endangered = 1;
                            break;
                        }
                    }
                    var num_children = cluster.getChildCount();
                    if (num_children == 1) {
                        num_children = "";
                    }
                    if (endangered) {
                        return L.divIcon({html: num_children, className: 'endangered', iconSize: 15});
                    } else {
                        return L.divIcon({html: num_children, className: 'not_endangered', iconSize: 15});
                    }


                }, singleMarkerMode: 1
            }
        );
clusters.addTo(map);


layers_array = [];
function plotSpecies() {
    $('#loader-plot').show();
    taxons_selected = {};
    for (i = 0; i < init_desc.length; i++) {
        if (init_desc[i].selected) {
            taxons_selected[init_desc[i].data.name] = {"index": init_desc[i].data.index, "taxLevel": init_desc[i].data.taxLevel};
        }
    }

    $.ajax({
            processData: false,
            type: 'POST',
            url: 'species/',
            data: JSON.stringify(taxons_selected),
            // data: {species_selected: $(".select2-species").select2('data')},
            contentType: false,  // add this to indicate 'multipart/form-data'
            success: function (data) {
                // layers_array.forEach(function(prev_layer) {
                //     clusters.removeLayer(prev_layer);
                //     prev_layer = null
                // });
                // data.forEach(function(species_path) {
                //     $.getJSON(static_path + species_path.path, function (geodata) {
                //         geodata = filterSpecies(geodata, species_path.species);
                //         var layer_curr = L.geoJSON(geodata, points_layer_options);
                //         layers_array.push(layer_curr);
                //     clusters.addLayer(layer_curr);
                //     });
                // });
                layers_array.forEach(function(prev_layer) {
                    clusters.removeLayer(prev_layer);
                    prev_layer = null
                });
                $.getJSON(static_path + "curr.geojson", function(geodata) {
                    var layer_curr = L.geoJSON(geodata, points_layer_options);
                        layers_array.push(layer_curr);
                    clusters.addLayer(layer_curr);
                });
                $('#loader-plot').hide();

            },
            error: function(data) {
                alert('Form submission failed');
                $('#loader-plot').hide();
            }});

}

function showSummary(mun_id, bbox) {
    $('#loader-summary').show();
    if (mun_id) {
        var curr_data = {"municipality": mun_id};
    }
    else {
        var curr_data = {"bbox": bbox};
    }
    $.ajax({
            processData: false,
            type: 'POST',
            url: 'summary/',
            data: JSON.stringify(curr_data),
            // data: {species_selected: $(".select2-species").select2('data')},
            contentType: false,  // add this to indicate 'multipart/form-data'
            success: function (data) {
                $.getJSON(static_path + "bar_sunburst.json", function(summary_json) {
                    // createSunburst(summary_json);
                    bar_chart_ref.redefine("data", summary_json);
                    sunburst_ref.redefine("data", summary_json);
                    document.getElementById('shiny').src = "http://127.0.0.1:7125/?municipality=" + mun_id;
                    document.getElementById('shiny').contentWindow.location.reload();
                });
                $('#loader-summary').hide();
            },
            error: function(data) {
                alert('Form submission failed');
                $('#loader-summary').hide();
            }});

}

function summariseSelection() {
    // return as x_min, ymin, x_max, y_max
    var curr_bounds = map.getBounds();
    showSummary( null,[curr_bounds.getNorthEast().lng, curr_bounds.getNorthEast().lat,
        curr_bounds.getSouthWest().lng, curr_bounds.getSouthWest().lat]);

}

// function filterSpecies(geodata, species) {
//     features = [];
//     geodata.features.forEach(function(row) {
//         if (row.properties.species == species) {
//             features.push(row);
//         }
//     });
//     geodata.features = features;
//     return features;
// }

function searchTaxon() {
    term = $("#taxon-search-field").val()
    results = [];
    for (i = 0; i < init_desc.length; i++) {
        if (term.toLowerCase() == init_desc[i].data.name.toLowerCase()) {
            results.push(init_desc[i])
        }

    }
    results.forEach( function(result) {
            while (result.parent) {
                if (result.parent._children) {
                    clickTree(result.parent);
                }
                result = result.parent;
            }
        }
    )

}








