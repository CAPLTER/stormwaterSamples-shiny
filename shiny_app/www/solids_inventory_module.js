
function samples_inventory_module_js(ns_prefix) {

  $("#" + ns_prefix + "solids_inventory_view").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "solid_id_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  $("#" + ns_prefix + "solids_inventory_view").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "solid_id_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  $("#" + ns_prefix + "solids_inventory_view").on("click", ".info_btn", function() {
    Shiny.setInputValue(ns_prefix + "sample_id_to_populate", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

}
