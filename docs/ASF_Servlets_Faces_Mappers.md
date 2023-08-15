## Faces Pretty URLs
The faces servlet supports pretty URLs with a custom XML configuration.
The `url-mapping` XML description allows to define a URL pattern that
contains parameters that will be injected in some Ada bean and will be
mapped to a specific faces XHTML view file.

```Ada
<url-mapping>
  <pattern>/wikis/#{wiki.wiki_space_id}/admin/#{wiki.page_id}/view.html</pattern>
  <view-id>/wikis/admin/view.html</view-id>
</url-mapping>

```

