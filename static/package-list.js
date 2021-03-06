// Functions related to package listings as produced by `package-summary-table` in site.rkt

function toggleBulkOperationSelections() {
  var checkboxes = Array.from(document.querySelectorAll("input.selected-packages"));
  var anySelected = checkboxes.some(function (n) { return n.checked; });
  var newState = anySelected ? false : true;
  checkboxes.forEach(function (n) { n.checked = newState; });
}

$(function() {
  "use strict";

  function applyFilter() {
    $("table.packages > tbody > tr").each(function() {
      var row = this;
      if (Number.parseInt($(row).data("todokey"), 10) === 0) {
        row.style.display = "none";
      }
    });
    $("table.packages").trigger("sorton", [[[4, 1]]]);
  }

  function removeFilter() {
    $("table.packages > tbody > tr").each(function() {
      var row = this;
      if (Number.parseInt($(row).data("todokey"), 10) === 0) {
        row.style.display = "";
      }
    });
    $("table.packages").trigger("sorton", [[[1, 0]]]);
  }

  var todoTotal = $("table.packages").data("todokey");

  if (todoTotal > 0) {
    $("#todo-msg").show();
    $("#todo-msg").html(
      todoTotal + " todos. " +
      "<a style='cursor:pointer' id='filter-pkgs'> Click here to see them.</a>"
    );

    var filterIsApplied = false;

    $("#filter-pkgs").click(function() {
      var filterLink = $(this);
      if (!filterIsApplied) {
        applyFilter();
        filterLink.text("Click to see all packages.");
        filterIsApplied = true;
      } else {
        removeFilter();
        filterLink.text("Click here to see them.");
        filterIsApplied = false;
      }
    });
  } else {
    $("#todo-msg").hide();
  }

}); /* document.ready */
