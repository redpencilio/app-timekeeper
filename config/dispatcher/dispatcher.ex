defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: [ "text/html", "application/xhtml+html" ],
    json: [ "application/json", "application/vnd.api+json" ],
    any: [ "*/*" ]
  ]

  define_layers [ :static, :web_page, :services, :fall_back, :not_found ]

  @json %{ accept: %{ json: true } }

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
  get "/customers/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/customers/"
  end

  get "/tasks/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/tasks/"
  end

  get "/workspaces/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/workspaces/"
  end

  match "/work-logs/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/work-logs/"
  end

  match "/timesheets/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/timesheets/"
  end

  get "/people/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/people/"
  end

  get "/user-groups/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/user-groups/"
  end

  match "/accounts/current/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://registration/accounts/current/"
  end

  match "/accounts/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/accounts/"
  end

  match "/sessions/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://login/sessions/"
  end

  match "/holiday-counters/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/holiday-counters/"
  end

  match "/concepts/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/concepts/"
  end

  match "/concept-schemes/*path", %{ layer: :services, accept: %{ json: true } } do
    forward conn, path, "http://cache/concept-schemes/"
  end

  match "/*_", %{ layer: :not_found } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
