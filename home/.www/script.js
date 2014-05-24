/* Get a link to today's and yesterday's ##hplusroadmap logs */
var base = 'http://gnusha.org/logs/'
var today = new Date();
var yesterday = new Date();
yesterday.setDate(today.getDate()-1);

function pad(str) { if(str.length == 1) return '0'+str; else return str; }

function format(date) {
    return base + date.getFullYear().toString() + '-' +
      pad((date.getMonth()+1).toString()) + '-' + pad(date.getDate().toString()) +
      '.log';
}

/* When clicking on the name of a folder, show/hide its contents */
$(".folder-name").click(function() {
    console.log('clicke');
    $(".folder[folder-id=" + $(this).attr('id') + "]").toggle();
});

$(document).ready(function() {
    $('#today').attr('href', format(today));
    $('#yesterday').attr('href', format(yesterday));
    $('.folder').hide();
});
