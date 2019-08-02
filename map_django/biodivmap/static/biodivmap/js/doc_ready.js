$(document).ready(function () {
    //initialise free selection tool
    drawnItems = L.featureGroup().addTo(map);

    map.addControl(new L.Control.Draw({
        edit: {
            featureGroup: drawnItems,
            poly: {
                allowIntersection: false
            }
        },
        draw: {
            polygon: {
                allowIntersection: false,
                showArea: true
            },
            circle: false,
            marker: false,
            circlemarker: false,
            polyline: false,
        }
    }));


    map.on(L.Draw.Event.CREATED, function (event) {
        var layer = event.layer;

        drawnItems.addLayer(layer);
        curr_shape = layer.toGeoJSON();
        summarisePolygon();
    });
    // init menu toggle
    $("#menu-toggle").click(function (e) {
        e.preventDefault();
        $("#wrapper").toggleClass("toggled");
    });

    // set up toggles for charts in the side-bar
    summary_divs = [document.getElementById("bar-charts"),
        document.getElementById("sunburst"), document.getElementById("shiny")];
    toggle_buttons = ["#toggle-bar-charts", "#toggle-sunburst", "#toggle-time-series"];
    summary_divs[0].style.display = "none";
    summary_divs[1].style.display = "none";
    summary_divs[2].style.display = "none";

    $(toggle_buttons[0]).change(function () {
        if ($(this).prop('checked')) {
            summary_divs[0].style.display = "block";
        } else {
            summary_divs[0].style.display = "none";
        }
    });

    $(toggle_buttons[1]).change(function () {
        if ($(this).prop('checked')) {
            summary_divs[1].style.display = "block";
        } else {
            summary_divs[1].style.display = "none";
        }
    });

    $(toggle_buttons[2]).change(function () {
        if ($(this).prop('checked')) {
            summary_divs[2].style.display = "block";
        } else {
            summary_divs[2].style.display = "none";
        }
    });

    // collapse side bar on load
    $("#wrapper").toggleClass("toggled");

    // init tool tips
    $(function () {
        $('[data-toggle="tooltip"]').tooltip()
    })

});