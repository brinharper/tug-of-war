/* config.js
 * 
 * This file contains the code necessary to load the configuration
 * for the experiment.
 */

// Object to hold the experiment configuration. It takes as parameters
// the numeric codes representing the experimental condition and
// whether the trials are counterbalanced.
var Config = function (condition, counterbalance) {

    var letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
    var usedNames = [];

    // These are the condition and counterbalancing ids

    // condition is which one is first
    this.condition = condition;
    // counterbalance is whether or not to show the two at the same time
    this.counterbalance = counterbalance;

    // Whether debug information should be printed out
    this.debug = true;
    // The amount of time to fade HTML elements in/out
    this.fade = 200;
    // List of trial information object for each experiment phase
    this.scenarios = new Object();

    // Canvas width and height
    this.canvas_width = 300;
    this.canvas_height = 300;

    // Lists of pages and examples for each instruction page.  We know
    // the list of pages we want to display a priori.
    this.instructions = {
        pages: ["instructions-training-1"]
    };

    // The list of all the HTML pages that need to be loaded
    this.pages = [
        "trial.html", 
        "submit.html"];

    // Parse the JSON object that we've requested and load it into the
    // configuration
    this.parse_config = function (data) {
        this.tests = data["tests"];
        this.scenarios = shuffle(data["scenarios"]); //shuffles the array 
        this.questions = data["questions"] ;
        this.colors = data["colors"];
        this.makeNames();
    };

    // Load the experiment configuration from the server
    this.load_config = function () {
        var that = this;
        var jsonpath = "/static/json/games.json"
        $.ajax({
            dataType: "json",
            url: jsonpath,
            async: false,
            success: function (data) { 
                if (that.debug) {
                    console.log("Got configuration data");
                }
                that.parse_config(data);
            }
        });
    };

    this.makeNames = function() {
        for (var i = 0; i < this.scenarios.length; i++) {
            var sceneNames = {};
            for (var j = 0; j < this.scenarios[i].games.length; j++) {
                for (var k = 0; k < this.scenarios[i].games[j].team1.length; k++) {
                    if (!sceneNames.hasOwnProperty(this.scenarios[i].games[j].team1[k].toString())) {
                        while (true) {
                            var x = Math.floor((Math.random() * 26));
                            var y = Math.floor((Math.random() * 26));
                            var n = letters[x] + letters[y];
                            if (usedNames.indexOf(n) < 0) {
                                usedNames.push(n);
                                sceneNames[this.scenarios[i].games[j].team1[k].toString()] = n;
                                this.scenarios[i].games[j].team1[k] = n;
                                break;
                            }
                        }
                    } else {
                        var num = this.scenarios[i].games[j].team1[k];
                        this.scenarios[i].games[j].team1[k] = sceneNames[num.toString()];
                    }
                }
                for (var k = 0; k < this.scenarios[i].games[j].team2.length; k++) {
                    if (!sceneNames.hasOwnProperty(this.scenarios[i].games[j].team2[k].toString())) {
                        while (true) {
                            var x = Math.floor((Math.random() * 26));
                            var y = Math.floor((Math.random() * 26));
                            var n = letters[x] + letters[y];
                            if (usedNames.indexOf(n) < 0) {
                                usedNames.push(n);
                                sceneNames[this.scenarios[i].games[j].team2[k].toString()] = n;
                                this.scenarios[i].games[j].team2[k] = n;
                                break;
                            }
                        }
                    } else {
                        var num = this.scenarios[i].games[j].team2[k];
                        this.scenarios[i].games[j].team2[k] = sceneNames[num.toString()];
                    }
                }
            }
            for (var j = 0; j < this.scenarios[i].comments.length; j++) {
                this.scenarios[i].comments[j] = Mustache.render(this.scenarios[i].comments[j], sceneNames);
            }
            for (var j = 0; j < this.scenarios[i].questions.length; j++) {
                if (this.scenarios[i].questions[j] == 0) {
                    this.scenarios[i].subjects[j].player = sceneNames[this.scenarios[i].subjects[j].player.toString()];
                } else if (this.scenarios[i].questions[j] == 1) {
                    this.scenarios[i].subjects[j].player = sceneNames[this.scenarios[i].subjects[j].player.toString()];
                }
            }
        }
    }

    // Request from the server configuration information for this run
    // of the experiment
    this.load_config();
};
