$(document).ready(function () {
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

        $("#wrapper").toggleClass("toggled");

        $(function () {
          $('[data-toggle="tooltip"]').tooltip()
        })

    });