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
        maxZoom: 17,
        minZoom: 2
    });

var baseLayers = {
    "Grayscale": grayScaleBaseMap,
    "Streets": streetsBaseMap
};

var overlays = {};
var num_overlays = 1;

var map = L.map('map', {
    center: [49.263710, -123.259378],
    zoom: 10,
    layers: [streetsBaseMap, grayScaleBaseMap]
});

grayScaleBaseMap.addTo(map);

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
                alert("No results");
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
        // var marker = L.marker(latlng, {title: ""});
        var redList = feature.properties.redList;
        console.log(feature.properties);
        var markerPopUp = '<div class="popUpfeature">' + feature.properties.species + '<br/> <b>'
            + feature.properties.common.split(',')[0] + '</b>';
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

        return marker;
    }
};

var points_layer = L.geoJson({ "type": "FeatureCollection", "features": [{ "type": "Feature", "properties": { "gbifID": 255736378, "datasetKey": "4fa7b334-ce0d-4e88-aaae-2e0c138d049e", "occurrenceID": "URN:catalog:CLO:EBIRD_CAN:OBS81006245", "kingdom": "Animalia", "phylum": "Chordata", "class": "Aves", "order": "Charadriiformes", "family": "Laridae", "genus": "Larus", "species": "Larus glaucescens", "taxonRank": "SPECIES", "scientificName": "Larus glaucescens J.F.Naumann, 1840", "countryCode": "CA", "locality": "Vancouver--Spanish Banks", "publishingOrgKey": "e2e717bf-551a-4917-bdc9-4fa0f342c530", "decimalLatitude": 49.277941, "decimalLongitude": -123.22897, "eventDate": "2010-01-06T00:00:00Z", "day": 6.0, "month": 1.0, "year": 2010.0, "taxonKey": 2481196, "speciesKey": 2481196.0, "basisOfRecord": "HUMAN_OBSERVATION", "institutionCode": "CLO", "collectionCode": "EBIRD_CAN", "catalogNumber": "OBS81006245", "license": "CC0_1_0", "recordedBy": "obsr38830", "lastInterpreted": "2019-03-29T01:15:37.476Z", "issue": "COORDINATE_ROUNDED" }, "geometry": { "type": "Point", "coordinates": [ -123.22897, 49.277941 ] } },] },
                        );

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
        clusters.addLayer(points_layer);
        // var clusters_layer = map.addLayer(clusters);
        // overlays.Gbif = clusters;
        // if (countKeys(overlays) == num_overlays) {
        //     L.control.layers(baseLayers, overlays, {collapsed: false}).addTo(map);
        // }

gbif_parts_filenames = ["UBC2010.geojson", "UBC2014.geojson", "UBC2018.geojson"];
gbif_years = [];
var init_year = 2000;
for (init_year; init_year <= 2019; init_year++) {
    gbif_years.push(init_year);
}

map.addLayer(clusters);

function loadPointsJson(filename) {
    $.getJSON(filename, function (data) {

        // add GeoJSON layer to the map once the file is loaded
        points_layer.addData(data);
        clusters.addLayer(points_layer);
    });
}

gbif_years.forEach(function(file) {
    loadPointsJson("gbif/"+ file.toString() + ".geojson");
});

// gbif_parts_filenames.forEach(function(file) {
//     loadPointsJson("gbif_parts/"+ file);
// });


// $.getJSON("red.geojson",function(data){
//     var redStar = L.icon({
//     iconUrl: 'red_star.png',
//     iconSize: [20,20]
//   });
//   L.geoJson(data  ,{
//     pointToLayer: function(feature,latlng){
// 	  return L.marker(latlng,{icon: redStar, riseOnHover: 1});
//     }
//   }  ).addTo(map);
// });

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

// "#FF851B", "#01FF70","#FFDC00" ,"#F012BE",

// var shapes_layer;
// $.getJSON("SEI.topojson", function (data) {
//     shapes_layer = L.topoJson(data, {
//
//         style: function (feature) {
//             sstyle = remove_highlight;
//             if (feature.properties.SE_ME_1 != "SE") {
//                 sstyle.fillColor = typeEco[feature.properties.SE_ME_1];
//             } else {
//                 sstyle.fillColor = typeSE[feature.properties.SECl_1];
//             }
//
//             return sstyle;
//         },
//         onEachFeature: function (feature, layer) {
//             var popUpInfo = feature.properties.Comp1Lgnd_;
//             if (feature.properties.Comp2Lgnd_) {
//                 popUpInfo += "<br/>" + feature.properties.Comp2Lgnd_;
//                 if (feature.properties.Comp3Lgnd_) {
//                     popUpInfo += "<br/>" + feature.properties.Comp3Lgnd_;
//                 }
//             }
//             popUpInfo += "</br> Quality: " + feature.properties.QualityNo_ + "/5.0";
//             if (feature.properties.Location) {
//                 popUpInfo += "</br> Location: " + feature.properties.Location;
//             }
//             // var popUp = layer.bindPopup(popUpInfo);
//             // popUp.on('popupclose', function() {
//             //     // layer.setStyle(remove_highlight);
//             // });
//             layer.on('click', function () {
//                 layer.bringToFront();
//                 if (layer.selected) {
//                     sstyle = remove_highlight;
//                     if (feature.properties.SE_ME_1 != "SE") {
//                         sstyle.fillColor = typeEco[feature.properties.SE_ME_1];
//                     } else {
//                         sstyle.fillColor = typeSE[feature.properties.SECl_1];
//                     }
//                     layer.setStyle(sstyle);
//                     layer.selected = 0;
//                     layer.unbindPopup();
//                 } else {
//                     sstyle = highlight;
//                     // if (feature.properties.SE_ME_1 != "SE") {
//                     //     sstyle.fillColor = typeEco[feature.properties.SE_ME_1];
//                     // }
//                     // else {
//                     //     sstyle.fillColor = typeSE[feature.properties.SECl_1];
//                     // }
//                     layer.setStyle(sstyle);
//                     layer.selected = 1;
//                     layer.bindPopup(popUpInfo).openPopup();
//                 }
//             })
//         }
//     })
//         .addTo(map);
//     overlays.SEI = shapes_layer;
//     if (countKeys(overlays) == num_overlays) {
//         L.control.layers(baseLayers, overlays, {collapsed: false}).addTo(map);
//     }
// });


// var htmlLegend1and2 = L.control.htmllegend({
//         position: 'bottomright',
//         legends: [{
//             name: 'Layer 1',
//             layer: points_layer,
//             elements: [{
//                 label: 'Rectangle',
//                 html: '',
//                 style: {
//                     'background-color': 'red',
//                     'width': '10px',
//                     'height': '10px'
//                 }
//             }, {
//                 label: 'Triangle',
//                 html: '',
//                 style: {
//                     'background-color': 'green',
//                     'width': '10px',
//                     'height': '10px'
//                 }
//             }]
//         }],
//         collapseSimple: true,
//         detectStretched: true,
//         collapsedOnInit: true,
//         defaultOpacity: 0.7,
//         visibleIcon: 'icon icon-eye',
//         hiddenIcon: 'icon icon-eye-slash'
//     });
//     map.addControl(htmlLegend1and2);
//
//
// var DeleteControl  = L.Control.extend({
//         options: {
//             position: 'topright'
//         },
//         onAdd: function(m) {
//             var btn = L.DomUtil.create('button')
//             btn.innerText = 'Delete layer 1'
//             L.DomEvent.on(btn, 'click', function() {
//                 // Remove legend uses entry id, which can be accessed through `htmllegend._entries`
//                 htmlLegend1and2.removeLegend(1)
//                 m.removeControl(this)
//             })
//             return btn
//         }
//     })
//     map.addControl(new DeleteControl())

//
//     L.control.layers(null, overlays, { collapsed: false }).addTo(map);