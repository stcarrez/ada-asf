
function Refresh(id) {
  jQuery.ajax({
    type: "GET",
	url: "api/monitor/" + id,
	data: null,
	context: document.body,
	success: function(data, status, jqXHDR) {
	    var response = {
	    error: null,
                    message: null,
                    status: jqXHDR.status,
                    contentType: jqXHDR.getResponseHeader('Content-type'),
                    location: jqXHDR.getResponseHeader('Location'),
                    data: data
                };
            /* callback(response); */
        }
  });
}

function Add_Value(id, value) {
  jQuery.ajax({
    type: "PUT",
	url: "api/monitor/" + id + "?value=" + value,
	data: "value=" + value,
	context: document.body,
	success: function(data, status, jqXHDR) {
	var response = {
	  error: null,
                    message: null,
                    status: jqXHDR.status,
                    contentType: jqXHDR.getResponseHeader('Content-type'),
                    location: jqXHDR.getResponseHeader('Location'),
                    data: data
                };
	Refresh(id)
	/*                callback(response);*/
      }
    });
}

function Update_Graph() {
  Add_Value(1, 123);
  setTimeout(function() { Update_Graph(); }, 10000);
}

Update_Graph();
