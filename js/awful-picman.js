set_pic_info_ro = function(id) {
    $('#ro-' + id).show();
    $('#rw-' + id).hide();
}

set_pic_info_rw = function(id) {
    $('#ro-' + id).hide();
    $('#rw-' + id).show();
}

get_tags = function() {
    var j;
    $.getJSON('/db/tags', function(json) {
	j = json;
    });
    return j;
}
