function imgResults(callback, speciesName) {
    var img_name = callback.entities;
    var key_entity = Object.keys(img_name)[0];
    img_name = callback.entities[key_entity].claims.P18[0].mainsnak.datavalue.value;

    img_name = img_name.replace(/\s+/g, '_');
    var img_name_hash = md5(img_name);
    var img_url = 'https://upload.wikimedia.org/wikipedia/commons/' + img_name_hash[0] +
        '/' + img_name_hash.substr(0, 2) + '/' + img_name;
    var speciesId = speciesName.replace(/\s+/g, '_');

    var img = new Image(),
        url = img_url,
        container = document.getElementById(speciesId + "_thumb");

    img.onload = function () {
        container.appendChild(img);
    };
    img.src = url;
}

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

function wikiImg(pageTitle, speciesName) { //AJAX request
    $.ajax({
        url: "https://www.wikidata.org/w/api.php?action=wbgetentities&format=json&sites=enwiki&props=claims&titles=" + pageTitle,
        dataType: "jsonp",
        success: function (response) {
            imgResults(response, speciesName);
        },
        error: function () {
            alert("Error retrieving search results, please refresh the page");
        }
    });
}

function loadImg(speciesName) { //AJAX request
    $.ajax({
        url: "https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=" + speciesName + "&prop=info&inprop=url&utf8=&format=json",
        dataType: "jsonp",
        success: function (response) {
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

            markerPopUp += '<input type="button" class="wikiButton" value="Loading" id=' + (id_species + "_wiki") + '>';

            markerPopUp += '<div class="thumbnail"  id=' + (id_species + "_thumb") + '> </div>';

            markerPopUp += '</div>';

            marker.bindPopup(markerPopUp);

            marker.on('click', function () {
                loadImg(feature.properties.species);
            });
        }
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
                return L.divIcon({html: num_children, className: 'endangered', iconSize: 20});
            } else {
                return L.divIcon({html: num_children, className: 'not_endangered', iconSize: 20});
            }


        }, singleMarkerMode: 1
    }
);
clusters.addTo(map);

