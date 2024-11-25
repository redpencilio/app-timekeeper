;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*) ;; enable if delta messages should be logged on terminal
(add-delta-messenger "http://delta-notifier/")
(setf *log-delta-messenger-message-bus-processing* nil) ;; set to t for extra messages for debugging delta messenger

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* nil) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil) ; change nil to t for logging all incoming requests

;;;;;;;;;;;;;;;;
;;; prefix types
(in-package :type-cache)

(add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session") ; each session URI will be handled for updates as if it had this mussession:Session type

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

;; these three reset the configuration, they are likely not necessary
(defparameter *access-specifications* nil)
(defparameter *graphs* nil)
(defparameter *rights* nil)

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
  :xsd "http://www.w3.org/2001/XMLSchema#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :wf "http://www.w3.org/2005/01/wf/flow#"
  :cal "http://www.w3.org/2002/12/cal/ical#"
  :foaf "http://xmlns.com/foaf/0.1/"
  :prov "http://www.w3.org/ns/prov#")


;;;;;;;;;
;; Graphs
;;
;; These are the graph specifications known in the system.  No
;; guarantees are given as to what content is readable from a graph.  If
;; two graphs are nearly identitacl and have the same name, perhaps the
;; specifications can be folded too.  This could help when building
;; indexes.

(define-graph timesheet ("http://mu.semte.ch/graphs/redpencil")
  ("cal:Vevent" -> _)
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _))

(define-graph static ("http://mu.semte.ch/graphs/redpencil")
  ("skos:ConceptScheme" -> _))

(define-graph kimai ("http://mu.semte.ch/graphs/kimai")
  ("prov:Organization" -> _)
  ("wf:Task" -> _))

;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(grant (read write)
       :to-graph timesheet
       :for-allowed-group "public")

(grant (read)
       :to-graph (static kimai)
       :for-allowed-group "public")

(with-scope "http://services.redpencil.io/timekeeper-kimai-sync-service"
  (grant (read write)
    :to-graph (kimai)
    :for-allowed-group "public"))

;; example:

;; (supply-allowed-group "company"
;;   :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
;;           SELECT DISTINCT ?uuid WHERE {
;;             <SESSION_ID ext:belongsToCompany/mu:uuid ?uuid
;;           }"
;;   :parameters ("uuid"))

;; (grant (read write)
;;        :to company
;;        :for "company")
