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
(add-type-for-prefix "http://timekeeper.redpencil.io/work-logs/" "http://www.w3.org/2002/12/cal/ical#Vevent") ; kimai-sync service needs to cleanup dangling triples after rdf:type has already been removed

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
  :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
  :xsd "http://www.w3.org/2001/XMLSchema#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :wf "http://www.w3.org/2005/01/wf/flow#"
  :cal "http://www.w3.org/2002/12/cal/ical#"
  :foaf "http://xmlns.com/foaf/0.1/"
  :dct "http://purl.org/dc/terms/"
  :prov "http://www.w3.org/ns/prov#"
  :qudt "http://qudt.org/schema/qudt/"
  :tempo "http://purl.org/tempo/"
  :doap "http://usefulinc.com/ns/doap#"
  :ui "http://www.w3.org/ns/ui#")


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
  ("skos:Collection" -> _)
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _))

(define-graph timesheet-admin ("http://mu.semte.ch/graphs/redpencil")
  ("qudt:Quantity"
    -> "qudt:maxInclusive"
    -> "prov:wasAssociatedWith"
    -> "qudt:hasQuantityKind"
    -> "tempo:validFrom"
    -> "tempo:validTo"))

(define-graph static ("http://mu.semte.ch/graphs/public")
  ("skos:ConceptScheme" -> _)
  ("skos:Concept" -> _))

(define-graph kimai ("http://mu.semte.ch/graphs/kimai")
  ("prov:Organization" -> _)
  ("wf:Task"
    -> "mu:uuid"
    -> "rdf:type"
    -> "rdfs:label"
    -> "skos:inScheme"
    -> "skos:topConceptOf"
    -> "skos:broader"
    -> "dct:identifier"
    -> "ui:color"
    -> "prov:wasAttributedTo")
  ("doap:Project"
    -> "mu:uuid"
    -> "rdf:type"
    -> "rdfs:label"
    -> "skos:inScheme"
    -> "skos:topConceptOf"
    -> "dct:identifier"
    -> "ui:color"
    -> "prov:wasAttributedTo")
  ("ext:KimaiActivity"
    -> "mu:uuid"
    -> "rdf:type"
    -> "rdfs:label"
    -> "skos:inScheme"
    -> "skos:broader"
    -> "dct:identifier"
    -> "ui:color")
  ("foaf:OnlineAccount" -> _))

(define-graph users ("http://mu.semte.ch/graphs/users")
  ("foaf:Person"
    -> "foaf:name"
    -> "foaf:account"
    -> "dct:created"
    -> "dct:modified"
    <- "ext:taskVisibleTo")
  ("foaf:OnlineAccount"
    -> "foaf:accountName"
    -> "foaf:accountServiceHomepage"
    -> "dct:created"
    -> "dct:modified")
  ("foaf:Group"
    -> "foaf:member"))

;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(supply-allowed-group "logged-in"
  :parameters ()
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      SELECT ?account WHERE {
          <SESSION_ID> session:account ?account .
      } LIMIT 1")

(supply-allowed-group "admin"
  :parameters ()
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      SELECT ?account WHERE {
          <SESSION_ID> session:account ?account .
          ?user foaf:account ?account .
          <http://mu.semte.ch/user-groups/admin> foaf:member ?user .
      } LIMIT 1")


(supply-allowed-group "employee"
  :parameters ()
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      SELECT ?account WHERE {
          <SESSION_ID> session:account ?account .
          ?user foaf:account ?account .
          <http://mu.semte.ch/user-groups/employee> foaf:member ?user .
      } LIMIT 1")

(grant (read)
       :to-graph (static)
       :for-allowed-group "public")

(with-scope "http://services.redpencil.io/timekeeper-kimai-sync-service"
  (grant (read write)
    :to-graph (kimai timesheet)
    :for-allowed-group "public")
  (grant (read)
    :to-graph (users)
    :for-allowed-group "public"))

(grant (read write)
       :to-graph (timesheet)
       :for-allowed-group "employee")

(grant (read)
       :to-graph (static kimai users timesheet-admin)
       :for-allowed-group "logged-in")

(grant (read write)
       :to-graph (users timesheet-admin)
       :for-allowed-group "admin")
