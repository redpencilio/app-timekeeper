defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ]
    any: [ "*/*" ]
  ]

  define_layers [ :static, :web_page, :services, :fall_back, :not_found ]

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule:
  #
  # match "/themes/*path", @json do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end
  #
  # Run `docker-compose restart dispatcher` after updating
  # this file.

  ###############
  # STATIC
  ###############
  get "/assets/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  get "/favicon.ico", %{ layer: :static } do
    send_resp( conn, 404, "" )
  end

  #################
  # FRONTEND PAGES
  #################
  get "/*path", %{ layer: :web_page, accept: %{ html: true } } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  ###############
  # API SERVICES
  ###############
  get "/projects/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://resource/projects/"
  end

  get "/sub-projects/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://resource/sub-projects/"
  end

  get "/work-logs/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://resource/work-logs/"
  end

  get "/customers/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://resource/customers/"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
