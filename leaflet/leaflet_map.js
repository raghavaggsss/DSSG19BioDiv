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


function ajax (keyword) { //AJAX request
	$.ajax({
		url: "https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=" + keyword + "&prop=info&inprop=url&utf8=&format=json",
		dataType: "jsonp",
		success: function(response) {
			// console.log(response.query);
			if (response.query.searchinfo.totalhits === 0) {
				alert("No results");
			}

			else {
				showResults(response);
			}
		},
		error: function () {
			alert("Error retrieving search results, please refresh the page");
		}
	});
}

function showResults(callback) {
    var title = callback.query.search[0].title;
    var url = title.replace(/ /g, "_");
    window.open("https://en.wikipedia.org/wiki/" + url, "_blank");
}



// openNewWindow = function(species)
// {
//
//  window.open("http://www.google.com/", "_blank");
// };

// load GeoJSON from an external file
// var points_data;
$.getJSON("gbif_tot.geojson", function (data) {
    var dotIcon = L.icon({
      iconUrl: 'blue_dot.png',
      iconSize: [10,10]
    });
    // add GeoJSON layer to the map once the file is loaded
    var points_data = L.geoJson(data, {
        pointToLayer: function (feature, latlng) {
            var marker = L.marker(latlng, {icon: dotIcon});
            // var marker = L.marker(latlng, {title: ""});
            var redList = feature.properties.redList;
            var markerPopUp = '<div class="popUpfeature">' + feature.properties.species + '<br/> <b>'
                + feature.properties.common + '</b>';
            if (redList) {
                markerPopUp += '<div class="redListFont">' + '<br/>' + feature.properties.redList + '</div>';
            }

            markerPopUp += '<br/>';

            markerPopUp += '<input type="button" value="Wiki" onClick="ajax(\'' + feature.properties.species + '\')" />';

            markerPopUp += '</div>';

            marker.bindPopup(markerPopUp);
            return marker;
        }
    });
    var clusters = L.markerClusterGroup(
        {iconCreateFunction: function(cluster) {
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
                return L.divIcon({ html: num_children, className: 'endangered', iconSize: 15});
            }
            else {
                return L.divIcon({ html: num_children, className: 'not_endangered', iconSize: 15 });
            }


	}, singleMarkerMode: 1}
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


// add shapes
$.getJSON("UBC_poly.geojson", function (hoodData) {
    L.geoJson(hoodData, {
        style: function (feature) {
            return {color: "#e10c1c", weight: 1, fillColor: "#66ff33", fillOpacity: .3};
        },
        onEachFeature: function (feature, layer) {
            layer.bindPopup("<strong>" + feature.properties.PROTECTED_ + "</strong><br/>")
        }
    })
        .addTo(map);
});
