$(document).ready(function () {

  $("form.languageSelector input[type=submit]").hide();

  $("form.languageSelector").submit(function () {
    return true;
  });

  $("select#lang").change(function () {
    $("form.languageSelector").submit();
  });

});
