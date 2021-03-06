Course Recommendation App
========================================================
author: Jamie Harwood
date: Sun Feb 22 02:09:40 2015
transition: rotate
css: CourseRecommender.css

"Taking the guesswork out of choosing an online course"

The Problem
========================================================


  - There are a growing number of courses available on MOOCS (Massive Open Online Course) platforms such as Coursera. How do you decide which course is right for you?

  - The course descriptions are often short and course content often not available until you have signed up

  - Wouldn't it be nice if, when you stumbled across an interesting article or tutorial you could use the information in that resource to automatically choose your next course with Coursera?

  - My Recommendatation App does just that!


How it works
========================================================

  - A random forest machine learning algorithm is used to train a model using previous course content  
  - This model is then used to classify new material provided by the user in the form of a url
  - The url data is downloaded and prepared so it can be fitted to the model  
  - A recommendation for a module within the Data Science specialization is given along with an accuracy measure
  - Content which does not appear to be about data science is flagged


Word cloud feature
========================================================

<div style="font-size: 0.75em;line-height:1.5">As well as a prediction the user is given a comparison of two word clouds like the one below so they can visually assess the similarity between the suggested course and the data they provided.  The word clouds are made up of the most frequent terms found in the content</div><center>
![plot of chunk unnamed-chunk-1](CourseRecommender-figure/unnamed-chunk-1-1.png) 

</center>

RChart feature
========================================================

<div style="font-size: 0.75em;line-height:1.5">The user also sees two interactive line charts like this one so that they can compare the relative frequencies of those terms that were actually used in the model</div>

<script type='text/javascript' src=//code.jquery.com/jquery-1.9.1.min.js></script>
<script type='text/javascript' src=//code.highcharts.com/highcharts.js></script>
<script type='text/javascript' src=//code.highcharts.com/highcharts-more.js></script>
<script type='text/javascript' src=//code.highcharts.com/modules/exporting.js></script> 
 <style>
  .rChart {
    display: block;
    margin-left: auto; 
    margin-right: auto;
    width: 1000px;
    height: 400px;
  }  
  </style>
<div id = 'chart33ce55b9c4dc' class = 'rChart highcharts'></div>
<script type='text/javascript'>
    (function($){
        $(function () {
            var chart = new Highcharts.Chart({
 "dom": "chart33ce55b9c4dc",
"width":           1000,
"height":            400,
"credits": {
 "href": null,
"text": null 
},
"exporting": {
 "enabled": false 
},
"title": {
 "text": "Average Term frequencies per lecture for the course material" 
},
"yAxis": [
 {
 "title": {
 "enabled": true,
"text": "Average Term Frequency" 
} 
} 
],
"series": [
 {
 "data": [
 [
 "able",
             0 
],
[
 "access",
             0 
],
[
 "across",
             1 
],
[
 "actual",
             6 
],
[
 "add",
             5 
],
[
 "adding",
             7 
],
[
 "address",
             1 
],
[
 "advanced",
             1 
],
[
 "all",
             0 
],
[
 "allows",
             0 
],
[
 "already",
             1 
],
[
 "also",
             8 
],
[
 "altplot",
             0 
],
[
 "always",
             4 
],
[
 "amount",
             2 
],
[
 "analyses",
             7 
],
[
 "analysis",
             0 
],
[
 "another",
             4 
],
[
 "applications",
             0 
],
[
 "applied",
             3 
],
[
 "apply",
             0 
],
[
 "appropriate",
             1 
],
[
 "are",
             0 
],
[
 "area",
             2 
],
[
 "arguments",
             0 
],
[
 "around",
             2 
],
[
 "assets",
             0 
],
[
 "assistant",
             8 
],
[
 "associate",
             0 
],
[
 "associated",
             1 
],
[
 "assume",
             6 
],
[
 "author",
             0 
],
[
 "automatically",
             0 
],
[
 "available",
             0 
],
[
 "average",
            15 
],
[
 "back",
             4 
],
[
 "based",
             3 
],
[
 "basic",
             7 
],
[
 "begin",
             0 
],
[
 "best",
             8 
],
[
 "better",
             2 
],
[
 "big",
             8 
],
[
 "binary",
             9 
],
[
 "biostatistics",
             8 
],
[
 "bloomberg",
             0 
],
[
 "bloombergshieldpng",
             0 
],
[
 "bootstrap",
            25 
],
[
 "brian",
            19 
],
[
 "building",
             0 
],
[
 "but",
             0 
],
[
 "button",
             0 
],
[
 "caffo",
            17 
],
[
 "call",
            14 
],
[
 "called",
            12 
],
[
 "can",
             0 
],
[
 "careful",
             1 
],
[
 "case",
             8 
],
[
 "cases",
             4 
],
[
 "center",
             5 
],
[
 "centerimg",
             0 
],
[
 "change",
            36 
],
[
 "changes",
             5 
],
[
 "character",
             0 
],
[
 "children",
            32 
],
[
 "chunk",
             0 
],
[
 "class",
             6 
],
[
 "classcenter",
             0 
],
[
 "classplot",
             0 
],
[
 "classrimage",
             0 
],
[
 "code",
             8 
],
[
 "col",
            11 
],
[
 "collection",
             4 
],
[
 "common",
             1 
],
[
 "commonly",
             3 
],
[
 "community",
             0 
],
[
 "components",
             2 
],
[
 "computer",
             0 
],
[
 "connection",
             0 
],
[
 "consider",
            27 
],
[
 "contain",
             0 
],
[
 "contains",
             0 
],
[
 "content",
             0 
],
[
 "continued",
             3 
],
[
 "control",
             1 
],
[
 "course",
             0 
],
[
 "cover",
             4 
],
[
 "create",
             8 
],
[
 "created",
             1 
],
[
 "creating",
             0 
],
[
 "data",
           140 
],
[
 "datascientisttoolbox",
             0 
],
[
 "date",
             2 
],
[
 "days",
             1 
],
[
 "default",
             1 
],
[
 "define",
             9 
],
[
 "depend",
             2 
],
[
 "depends",
             5 
],
[
 "describe",
             6 
],
[
 "description",
             1 
],
[
 "design",
             3 
],
[
 "developingdataproducts",
             0 
],
[
 "development",
             4 
],
[
 "deviation",
             7 
],
[
 "difference",
             6 
],
[
 "differences",
             2 
],
[
 "different",
            17 
],
[
 "difficult",
             3 
],
[
 "directly",
             0 
],
[
 "discussion",
             9 
],
[
 "div",
             0 
],
[
 "document",
             2 
],
[
 "done",
             1 
],
[
 "dont",
            16 
],
[
 "download",
             0 
],
[
 "draft",
            25 
],
[
 "due",
             6 
],
[
 "dzslides",
            25 
],
[
 "each",
             0 
],
[
 "easier",
             2 
],
[
 "easy",
             0 
],
[
 "either",
             3 
],
[
 "elements",
             2 
],
[
 "else",
             3 
],
[
 "end",
             1 
],
[
 "equal",
             5 
],
[
 "equivalent",
             3 
],
[
 "error",
            31 
],
[
 "errors",
            16 
],
[
 "estimate",
            57 
],
[
 "estimated",
             8 
],
[
 "etc",
             4 
],
[
 "evaluate",
             2 
],
[
 "even",
             9 
],
[
 "every",
             5 
],
[
 "everything",
            17 
],
[
 "example",
             0 
],
[
 "examples",
             6 
],
[
 "expected",
            14 
],
[
 "exploratoryanalysis",
             0 
],
[
 "factor",
            10 
],
[
 "false",
             0 
],
[
 "file",
             2 
],
[
 "files",
             0 
],
[
 "final",
             2 
],
[
 "find",
             1 
],
[
 "first",
            12 
],
[
 "fit",
            44 
],
[
 "follow",
             4 
],
[
 "following",
             6 
],
[
 "for",
             0 
],
[
 "frame",
            11 
],
[
 "framework",
            25 
],
[
 "free",
             0 
],
[
 "function",
            18 
],
[
 "functions",
             4 
],
[
 "further",
             0 
],
[
 "future",
             0 
],
[
 "general",
             6 
],
[
 "generally",
             3 
],
[
 "get",
            23 
],
[
 "gets",
             4 
],
[
 "getting",
             3 
],
[
 "gettingdata",
             0 
],
[
 "give",
             1 
],
[
 "given",
             4 
],
[
 "gives",
             0 
],
[
 "goal",
             4 
],
[
 "going",
             0 
],
[
 "good",
             5 
],
[
 "google",
             0 
],
[
 "great",
             1 
],
[
 "group",
            33 
],
[
 "groups",
             2 
],
[
 "guide",
             0 
],
[
 "hard",
             2 
],
[
 "harder",
             2 
],
[
 "health",
             0 
],
[
 "height",
             0 
],
[
 "help",
             2 
],
[
 "here",
             0 
],
[
 "highlight",
             0 
],
[
 "highlighter",
             0 
],
[
 "highlightjs",
             0 
],
[
 "hitheme",
            25 
],
[
 "hopkins",
             0 
],
[
 "how",
             0 
],
[
 "however",
             4 
],
[
 "html",
             0 
],
[
 "htmlslides",
            25 
],
[
 "idea",
             1 
],
[
 "ideas",
             7 
],
[
 "img",
             0 
],
[
 "immediately",
             1 
],
[
 "important",
             2 
],
[
 "include",
            12 
],
[
 "including",
             9 
],
[
 "independent",
             3 
],
[
 "inference",
            11 
],
[
 "information",
             5 
],
[
 "instead",
             2 
],
[
 "interested",
             4 
],
[
 "interesting",
             1 
],
[
 "interpret",
             5 
],
[
 "intervals",
            16 
],
[
 "intrain",
             0 
],
[
 "introduction",
             1 
],
[
 "its",
             0 
],
[
 "jeff",
            16 
],
[
 "jeffrey",
             9 
],
[
 "job",
            25 
],
[
 "johns",
             0 
],
[
 "just",
             6 
],
[
 "keep",
             2 
],
[
 "key",
             6 
],
[
 "know",
            17 
],
[
 "language",
             0 
],
[
 "large",
             7 
],
[
 "last",
             3 
],
[
 "later",
             2 
],
[
 "lead",
             1 
],
[
 "learning",
             2 
],
[
 "least",
            25 
],
[
 "lecture",
             5 
],
[
 "leek",
            26 
],
[
 "left",
             5 
],
[
 "length",
             6 
],
[
 "less",
             4 
],
[
 "let",
             3 
],
[
 "level",
            10 
],
[
 "levels",
             3 
],
[
 "lib",
            25 
],
[
 "libraries",
            18 
],
[
 "librariesnew",
             7 
],
[
 "librarycaret",
             0 
],
[
 "libraryusingr",
             8 
],
[
 "like",
             9 
],
[
 "line",
            43 
],
[
 "linear",
            86 
],
[
 "link",
             9 
],
[
 "list",
             0 
],
[
 "listfalse",
             0 
],
[
 "little",
             3 
],
[
 "load",
             1 
],
[
 "log",
            18 
],
[
 "logo",
            25 
],
[
 "look",
             6 
],
[
 "looking",
             3 
],
[
 "lot",
             2 
],
[
 "mac",
             0 
],
[
 "machine",
             3 
],
[
 "made",
             0 
],
[
 "main",
             1 
],
[
 "make",
             3 
],
[
 "makes",
             3 
],
[
 "making",
             0 
],
[
 "many",
            10 
],
[
 "mathjax",
             0 
],
[
 "may",
            13 
],
[
 "mean",
            44 
],
[
 "means",
             8 
],
[
 "measure",
            22 
],
[
 "message",
             0 
],
[
 "method",
             1 
],
[
 "methods",
             2 
],
[
 "might",
             2 
],
[
 "minimum",
             2 
],
[
 "mode",
            25 
],
[
 "model",
           110 
],
[
 "models",
            42 
],
[
 "more",
             0 
],
[
 "most",
             0 
],
[
 "much",
             4 
],
[
 "multiple",
            10 
],
[
 "must",
             5 
],
[
 "name",
             0 
],
[
 "names",
             0 
],
[
 "necessary",
             2 
],
[
 "need",
             5 
],
[
 "needed",
             4 
],
[
 "new",
             0 
],
[
 "next",
             3 
],
[
 "not",
             0 
],
[
 "note",
            20 
],
[
 "notes",
            10 
],
[
 "notice",
             4 
],
[
 "now",
             3 
],
[
 "null",
             4 
],
[
 "number",
            26 
],
[
 "object",
             2 
],
[
 "objects",
             0 
],
[
 "obtain",
             1 
],
[
 "obtained",
             1 
],
[
 "often",
            13 
],
[
 "one",
             0 
],
[
 "open",
             3 
],
[
 "order",
             3 
],
[
 "original",
             1 
],
[
 "other",
             0 
],
[
 "others",
             0 
],
[
 "our",
             0 
],
[
 "outcome",
            19 
],
[
 "outcomes",
            13 
],
[
 "overview",
             0 
],
[
 "package",
             0 
],
[
 "packages",
             1 
],
[
 "page",
             0 
],
[
 "paper",
             4 
],
[
 "parameters",
             6 
],
[
 "part",
             3 
],
[
 "particular",
             2 
],
[
 "particularly",
             1 
],
[
 "peng",
            16 
],
[
 "people",
             6 
],
[
 "place",
             0 
],
[
 "plot",
             0 
],
[
 "plotting",
             5 
],
[
 "point",
            30 
],
[
 "points",
            29 
],
[
 "population",
             2 
],
[
 "possible",
             2 
],
[
 "powerful",
             0 
],
[
 "practicalmachinelearning",
             0 
],
[
 "prediction",
            17 
],
[
 "predictors",
             1 
],
[
 "prettify",
            25 
],
[
 "previous",
             2 
],
[
 "primarily",
             0 
],
[
 "primary",
             1 
],
[
 "probability",
            14 
],
[
 "probably",
             2 
],
[
 "problem",
             4 
],
[
 "problems",
             3 
],
[
 "produce",
             0 
],
[
 "professor",
             8 
],
[
 "properties",
             2 
],
[
 "public",
             0 
],
[
 "put",
             1 
],
[
 "quick",
             0 
],
[
 "quickly",
             1 
],
[
 "quiz",
             0 
],
[
 "random",
             3 
],
[
 "rather",
             2 
],
[
 "read",
             1 
],
[
 "reading",
             0 
],
[
 "real",
             6 
],
[
 "regression",
           130 
],
[
 "regressionmodels",
             0 
],
[
 "related",
            13 
],
[
 "relationship",
            27 
],
[
 "relevant",
             4 
],
[
 "remember",
             4 
],
[
 "report",
             7 
],
[
 "reproducible",
             0 
],
[
 "reproducibleresearch",
             0 
],
[
 "requires",
             2 
],
[
 "resources",
             5 
],
[
 "result",
             1 
],
[
 "results",
            10 
],
[
 "review",
             1 
],
[
 "right",
            16 
],
[
 "roger",
            16 
],
[
 "rprogramming",
             0 
],
[
 "rules",
             2 
],
[
 "run",
             2 
],
[
 "sample",
             9 
],
[
 "samples",
             4 
],
[
 "say",
             8 
],
[
 "scale",
             2 
],
[
 "school",
             0 
],
[
 "science",
             1 
],
[
 "second",
             1 
],
[
 "see",
             8 
],
[
 "selfcontained",
            25 
],
[
 "sense",
             4 
],
[
 "series",
             0 
],
[
 "set",
             0 
],
[
 "sets",
             0 
],
[
 "setseed",
            14 
],
[
 "setting",
             2 
],
[
 "shower",
            25 
],
[
 "similar",
             1 
],
[
 "simple",
             4 
],
[
 "since",
             3 
],
[
 "size",
             4 
],
[
 "small",
             4 
],
[
 "some",
             0 
],
[
 "something",
             2 
],
[
 "sometimes",
             3 
],
[
 "source",
             0 
],
[
 "specific",
             3 
],
[
 "specify",
             0 
],
[
 "srcfigunnamedchunkpng",
             0 
],
[
 "standalone",
            25 
],
[
 "standard",
            31 
],
[
 "start",
             1 
],
[
 "starting",
             0 
],
[
 "statistical",
             7 
],
[
 "statisticalinference",
             0 
],
[
 "statistics",
             2 
],
[
 "step",
             8 
],
[
 "steps",
             0 
],
[
 "still",
             5 
],
[
 "structure",
             0 
],
[
 "study",
             1 
],
[
 "subtitle",
            25 
],
[
 "summary",
             7 
],
[
 "suppose",
             4 
],
[
 "system",
             0 
],
[
 "table",
             6 
],
[
 "take",
             7 
],
[
 "takes",
             0 
],
[
 "taking",
             6 
],
[
 "terms",
            14 
],
[
 "test",
             3 
],
[
 "testing",
             2 
],
[
 "text",
             2 
],
[
 "the",
             0 
],
[
 "then",
             0 
],
[
 "there",
             0 
],
[
 "therefore",
             1 
],
[
 "these",
             0 
],
[
 "they",
             0 
],
[
 "thing",
             2 
],
[
 "things",
            17 
],
[
 "think",
             4 
],
[
 "this",
             0 
],
[
 "three",
             3 
],
[
 "time",
             8 
],
[
 "times",
             9 
],
[
 "title",
             0 
],
[
 "titleplot",
             0 
],
[
 "together",
             0 
],
[
 "tomorrow",
            25 
],
[
 "tool",
             1 
],
[
 "tools",
             1 
],
[
 "top",
             1 
],
[
 "training",
             0 
],
[
 "true",
             2 
],
[
 "try",
             3 
],
[
 "two",
            21 
],
[
 "type",
             6 
],
[
 "types",
             0 
],
[
 "understanding",
             2 
],
[
 "unnamedchunk",
             0 
],
[
 "url",
             0 
],
[
 "use",
             0 
],
[
 "used",
             8 
],
[
 "useful",
             6 
],
[
 "uses",
             1 
],
[
 "using",
            14 
],
[
 "usually",
             2 
],
[
 "value",
            34 
],
[
 "values",
            24 
],
[
 "variable",
            20 
],
[
 "variables",
            42 
],
[
 "variance",
            40 
],
[
 "vector",
             0 
],
[
 "version",
             0 
],
[
 "versus",
            13 
],
[
 "via",
            10 
],
[
 "view",
             0 
],
[
 "want",
             9 
],
[
 "way",
            11 
],
[
 "ways",
             3 
],
[
 "web",
             4 
],
[
 "well",
            10 
],
[
 "what",
             0 
],
[
 "when",
             0 
],
[
 "whether",
             1 
],
[
 "whole",
             0 
],
[
 "why",
             0 
],
[
 "widgets",
            25 
],
[
 "will",
             0 
],
[
 "windows",
             0 
],
[
 "with",
             0 
],
[
 "without",
             3 
],
[
 "work",
             5 
],
[
 "working",
             1 
],
[
 "works",
             5 
],
[
 "write",
             3 
],
[
 "writing",
             3 
],
[
 "written",
             1 
],
[
 "year",
            46 
],
[
 "you",
             0 
],
[
 "your",
             0 
],
[
 "zenburn",
             0 
],
[
 "zero",
            28 
] 
],
"type": "line",
"marker": {
 "radius":              3 
} 
} 
],
"legend": {
 "enabled": false 
},
"xAxis": [
 {
 "title": {
 "enabled": true,
"text": "Term index" 
} 
} 
],
"subtitle": {
 "text": null 
},
"colors": [ "orange", "lightgreen" ],
"id": "chart33ce55b9c4dc",
"chart": {
 "renderTo": "chart33ce55b9c4dc" 
} 
});
        });
    })(jQuery);
</script>

<div style="font-size: 0.75em;line-height:1.5">The recommender app is a quick, easy and fun way for the budding data scientist to find their next course of study.   </div>
<center>[Try it now for free!] (https://jamiefharwood.shinyapps.io/shinyApp/)</center>
