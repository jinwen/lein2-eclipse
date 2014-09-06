(ns leiningen.eclipse
  "Create Eclipse project descriptor files."
  (:use [clojure.java.io :only [file writer]])
  (:use [clojure.contrib.prxml :only [prxml *prxml-indent*]])
  (:require [clojure.string])
  (:use [leiningen.deps :only [deps]]
        [leiningen.core.classpath :only [get-classpath]])
  (:import [java.io File])
  (:import [java.util.regex Pattern]))

(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evalutes body.
  Anything printed within body will be written to f."
  [f & body]
  `(with-open [stream# (writer ~f)]
     (binding [*out* stream#]
       ~@body)))

;; copied from jar.clj
(defn- unix-path
  [path]
  (.replaceAll path "\\\\" "/"))

;; copied from jar.clj
(defn- trim-leading-str
  [s to-trim]
  (clojure.string/replace s (re-pattern (str "^" (Pattern/quote to-trim))) ""))

(defn- directory?
  [arg]
  (.isDirectory (File. arg)))

(defn- list-libraries
  [project]
  (map #(.getPath %) (.listFiles (File. (:library-path project)))))

(defn- create-classpath
  "Print .classpath to *out*."
  [project]
  (let [root (str (unix-path (:root project)) \/)
        noroot  #(trim-leading-str (unix-path %) root)
        [compile-path]
        (map noroot (map project [:compile-path]))
        [resource-paths source-paths test-paths]
        (map #(map noroot %) (map project [:resource-paths
                                           :source-paths
                                           :test-paths]))
        full-classpath   (get-classpath project)
        pruned-classpath (remove #(or (= compile-path %)
                                      (some #{%} (mapcat project [:resource-paths
                                                                  :source-paths
                                                                  :test-paths])))
                                 full-classpath)]
    (prxml [:decl!]
           [:classpath
            (map (fn [c] (when (directory? c) [:classpathentry {:kind "src" :path c}]))
                 source-paths)
            (map (fn [c] [:classpathentry {:kind "lib" :path c}])
                 pruned-classpath)
            (map (fn [c] (when (directory? c) [:classpathentry {:kind "src" :path c}]))
                 test-paths)
            (map (fn [c] (when (directory? c) [:classpathentry {:kind "src" :path c}]))
                 resource-paths)
            [:classpathentry {:kind "con"
                              :path "org.eclipse.jdt.launching.JRE_CONTAINER"}]
            [:classpathentry {:kind "output"
                              :path compile-path}]
            ])))

(defn- create-project
  "Print .project to *out*."
  [project]
  (prxml [:decl!]
         [:projectDescription
          [:name (:name project)]
          [:comment (:description project)]
          [:projects]
          [:buildSpec
           [:buildCommand
            [:name "ccw.builder"]
            [:arguments]]
           [:buildCommand
            [:name "org.eclipse.jdt.core.javabuilder"]
            [:arguments]]]
          [:natures
           [:nature "ccw.nature"]
           [:nature "org.eclipse.jdt.core.javanature"]]]))

(defn eclipse
  "Create Eclipse project descriptor files."
  [project]
  (deps project)
  (binding [*prxml-indent* 2]
    (with-out-writer
      (file (:root project) ".classpath")
      (create-classpath project))
    (println "Created .classpath")
    (with-out-writer
      (file (:root project) ".project")
      (create-project project))
    (println "Created .project")))
