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

// base layers
var cartodbAttribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="http://cartodb.com/attributions">CartoDB</a>';

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

// initialise map and add base layers
var map = L.map('map', {
    center: [49.263710, -123.259378],
    zoom: 10,
    layers: [streetsBaseMap, grayScaleBaseMap]
});

// initialise municipality layer

mun_layer = L.geoJson(false, {
    onEachFeature: function (feature, layer) {
        layer.bindPopup(feature.properties.FullName);

        layer.on('click', function () {
            // change to newID
            curr_shape = {'type': 'Feature', 'properties': {}, 'geometry': feature.geometry};
            summarisePolygon(null);
        });
        layer.setStyle(municipality_style);
    }

});

// add municipalities layer in the control group which has base layers
L.control.layers(baseLayers, {Municipalities: mun_layer}, {collapsed: false}).addTo(map);


// SEI info dicts
var typeSEI = {
    "ME": "#F012BE",
    // "SE": "#85144b",
    "XX": "rgb(39,39,48)",
    "YS": "#c9c731",
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

var seiFullNames = {
    "ME": "Modified Ecosystem",
    // "SE": "#85144b",
    "XX": "Other",
    "YS": "Young Forest",
    "OF": "Old Forest",
    "MF": "Mature Forest",
    "WD": "Woodland",
    "RI": "Riparian",
    "IT": "Intertidal & Shallow Sub-tidal",
    "WN": "Wetland",
    "HB": "Herbaceous",
    "SV": "Sparsely Vegetated",
    "ES": "Estuarine",
    "FW": "Freshwater Lakes & Ponds",
    "AP": "Alpine",
};

// create a control group for SEI
sei_control_group = L.control.layers();

// initialise sei layers
sei_layers = {};

function init_sei_layer(sei_type) {
    curr_sei_layer = L.geoJson(false, {
        style: function (feature) {
            sstyle = remove_highlight;
            if (feature.properties.SE_ME_1 != "SE") {
                sstyle.fillColor = typeSEI[feature.properties.SE_ME_1];
            } else {
                sstyle.fillColor = typeSEI[feature.properties.SECl_1];
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
            layer.on('click', function () {
                layer.bringToFront();
                if (layer.selected) {
                    sstyle = remove_highlight;
                    if (feature.properties.SE_ME_1 != "SE") {
                        sstyle.fillColor = typeSEI[feature.properties.SE_ME_1];
                    } else {
                        sstyle.fillColor = typeSEI[feature.properties.SECl_1];
                    }
                    layer.setStyle(sstyle);
                    layer.selected = 0;
                    layer.unbindPopup();
                } else {
                    sstyle = highlight;
                    layer.setStyle(sstyle);
                    layer.selected = 1;
                    layer.bindPopup(popUpInfo).openPopup();
                }
                curr_shape = {'type': 'Feature', 'properties': {}, 'geometry': feature.geometry};
                summarisePolygon(feature.properties.SEI_PolyNb);
            })
        }
    });

    // add each sei layer to sei control group
    sei_control_group.addOverlay(curr_sei_layer, '<b> <text style="color:' + typeSEI[sei_type] + ';">' + seiFullNames[sei_type] + "</text> </b>");
    sei_layers[sei_type] = curr_sei_layer;
}

Object.keys(typeSEI).forEach(function (sei_key) {
    init_sei_layer(sei_key);
});

// add SEI control group to map
sei_control_group.addTo(map);

// load data into municipality and SEI layer as per request
// does not load data on window onload
map.on('overlayadd', function (l) {
    if (l.layer == mun_layer) {
        json_path = "map_data/Municipalities.geojson";
    } else {
        Object.keys(sei_layers).forEach(function (sei_layer_name) {
            if (l.layer == sei_layers[sei_layer_name]) {
                json_path = "map_data/sei/" + sei_layer_name + ".geojson";
            }
        });

    }

    if (l.layer.toGeoJSON().features.length == 0) {
        $.getJSON(static_path + json_path, function (data) {
            l.layer.addData(data);
        });
    }

});



