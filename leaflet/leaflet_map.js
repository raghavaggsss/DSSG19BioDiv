// initialize the map
var map = L.map('map').setView([49.263710, -123.259378], 13);

// load a tile layer
// L.tileLayer('http://tiles.mapc.org/basemap/{z}/{x}/{y}.png',
//   {
//     attribution: 'Tiles by <a href="http://mapc.org">MAPC</a>, Data by <a href="http://mass.gov/mgis">MassGIS</a>',
//     maxZoom: 17,
//     minZoom: 9
//   }).addTo(map);

// openstreet maps
L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    {
        maxZoom: 17,
        minZoom: 2
    }).addTo(map);



function openWiki(callback, speciesName) {
    var title = callback.query.search[0].title;

    var url = title.replace(/ /g, "_");

    var speciesId = speciesName.replace(/\s+/g, '_');

    var wikiButton = document.getElementById(speciesId + "_wiki");
    wikiButton.onclick = function () {window.open("https://en.wikipedia.org/wiki/" + url, "_blank");};
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
        container = document.getElementById( speciesId + "_thumb");

    img.onload = function () { container.appendChild(img); };
    img.src = url;
    // return img_url;


}

// load GeoJSON from an external file
var points_data;
$.getJSON("gbif_tot.geojson", function (data) {
    var dotIcon = L.icon({
        iconUrl: 'blue_dot.png',
        iconSize: [10, 10]
    });
    // add GeoJSON layer to the map once the file is loaded
    var points_data = L.geoJson(data, {
        pointToLayer: function (feature, latlng) {
            var marker = L.marker(latlng, {icon: dotIcon});
            // var marker = L.marker(latlng, {title: ""});
            var redList = feature.properties.redList;
            var markerPopUp = '<div class="popUpfeature">' + feature.properties.species + '<br/> <b>'
                + feature.properties.common.split(',')[0] + '</b>';
            if (redList) {
                markerPopUp += '<b class="redListFont">' + '<br/>' + feature.properties.redList + '</b>';
            }

            markerPopUp += '<br/>';

            var id_species  = feature.properties.species;
            if (id_species) {
                id_species = id_species.replace(/\s+/g, '_');
            }

            // markerPopUp += '<input type="button" value="wikiImg" '+  ' onClick="loadImg(\'' + feature.properties.species + '\')" />';

            // markerPopUp += '<input type="button" value="Wiki" '+ ' onClick="goToWiki(\'' + feature.properties.species + '\')" />';

            markerPopUp += '<input type="button" class="wikiButton" value="Loading" id=' + (id_species + "_wiki") + '>';

            markerPopUp += '<div class="thumbnail"  id=' + (id_species + "_thumb") + '> </div>';

            markerPopUp += '</div>';

            marker.bindPopup(markerPopUp);

            marker.on('click', function() {
                loadImg(feature.properties.species);
            });

            return marker;
        }
    });
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
    clusters.addLayer(points_data);
    map.addLayer(clusters);
});

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
    'color': '#333333',
    'weight': 1,
    'opacity': 1,
    'fillColor': '#14bd00'
};

var remove_highlight = {
    'color': "#515055",
    'weight': 1,
    'fillColor': "#8d8c91",
    'fillOpacity': .5};


// add shapes
$.getJSON("SEI.geojson", function (data) {
    L.geoJson(data, {
        style: function (feature) {
            return remove_highlight;
        },
        onEachFeature: function (feature, layer) {
            var popUpInfo =  feature.properties.Comp1Lgnd_;
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
            layer.on('click', function() {
                layer.bringToFront();
                if (layer.selected) {
                    layer.setStyle(remove_highlight);
                    layer.selected = 0;
                    layer.unbindPopup();
                }
                else {
                    layer.setStyle(highlight);
                    layer.selected = 1;
                    layer.bindPopup(popUpInfo).openPopup();
                }
            })
        }
    })
        .addTo(map);
});


