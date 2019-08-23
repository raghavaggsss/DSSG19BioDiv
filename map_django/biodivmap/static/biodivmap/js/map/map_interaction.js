var bar_chart_occurrence_ref;
var bar_chart_species_ref;
var sunburst_ref;
var curr_shape;

layers_array = [];

function plotSpecies() {
    $('#loader-plot').show();
    taxons_selected = {};
    for (i = 0; i < init_desc.length; i++) {
        if (init_desc[i].selected) {
            taxons_selected[init_desc[i].data.name] = {
                "index": init_desc[i].data.index,
                "taxLevel": init_desc[i].data.taxLevel
            };
        }
    }
    var region_taxons = {"taxons": taxons_selected, "polygon": curr_shape};

    $.ajax({
        processData: false,
        type: 'POST',
        url: 'species/',
        data: JSON.stringify(region_taxons),
        contentType: false,
        success: function (response) {
            layers_array.forEach(function (prev_layer) {
                clusters.removeLayer(prev_layer);
                prev_layer = null
            });
            if (response["status"] == "success") {
                var layer_curr = L.geoJSON($.parseJSON(response["data"]), points_layer_options);
                layers_array.push(layer_curr);
                clusters.addLayer(layer_curr);

            } else if (response["status"] == "no occurrence") {
                alert("Selected organisms do not occur in the selected region")
            }
            $('#loader-plot').hide();


        },
        error: function (data) {
            alert('Plot species failed');
            $('#loader-plot').hide();
        }
    });

}

function summarisePolygon(sei_index) {
    $('#loader-summary').show();
    $.ajax({
        processData: false,
        type: 'POST',
        url: 'summarypolygon/',
        data: JSON.stringify({"shape": curr_shape, "sei_index": sei_index}),
        contentType: false,
        success: function (response) {
            {
                reload_tax_tree(response["summary"]);
                bar_chart_occurrence_ref.redefine("data", response["summary"]);
                bar_chart_species_ref.redefine("data", response["summary"]);
                sunburst_ref.redefine("data", response["summary"]);

                if (sei_index) {
                    var dynatable = $('#prediction-table').dynatable({
                      dataset: {
                        records: response["pred"]["records"]
                      }
                    }).data('dynatable');

                    dynatable.settings.dataset.originalRecords = response["pred"]["records"];
                    dynatable.process();
                    }

                $('#loader-summary').hide();

                document.getElementById('shiny').src = "http://127.0.0.1:4609/?coords=" + JSON.stringify(
                    curr_shape["geometry"]["coordinates"][0]
                );
                document.getElementById('shiny').contentWindow.location.reload();
            }

        },
        error: function (data) {
            alert('summary failed');
            $('#loader-summary').hide();
        }
    });
}

// function predictSEI(sei_index) {
//     $('#loader-summary').show();
//     $.ajax({
//         processData: false,
//         type: 'POST',
//         url: 'predict/',
//         data: JSON.stringify({"sei_index": sei_index}),
//         // data: {species_selected: $(".select2-species").select2('data')},
//         contentType: false,  // add this to indicate 'multipart/form-data'
//         // dataType: 'text',
//         success: function (species) {
//             {
//                 var dynatable = $('#prediction-table').dynatable({
//                   dataset: {
//                     records: species["records"]
//                   }
//                 }).data('dynatable');
//
//                 dynatable.settings.dataset.originalRecords = species["records"];
//                 dynatable.process();
//                     // .bind('dynatable:afterProcess', changeColor);
//
//                 // reload_tax_tree(summary_json);
//                 // // createSunburst(summary_json);
//                 // bar_chart_occurrence_ref.redefine("data", summary_json);
//                 // bar_chart_species_ref.redefine("data", summary_json);
//                 // sunburst_ref.redefine("data", summary_json);
//                 $('#loader-summary').hide();
//                 // document.getElementById('shiny').src = "http://127.0.0.1:4609/?coords=" + JSON.stringify(
//                 //     curr_shape["geometry"]["coordinates"][0]
//                 // );
//                 // document.getElementById('shiny').contentWindow.location.reload();
//
//             }
//
//         },
//         error: function (data) {
//             alert('summary failed');
//             $('#loader-summary').hide();
//         }
//     });
// }

// function changeColor() {
//     $('#prediction-table tr td').each(function() {
//         if ($(this).text() == 'yes') {
//             $(this).closest('tr').css('background-color', '#7fff7e');
//         }
//     });
// }