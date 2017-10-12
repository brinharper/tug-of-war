/* task.js
 * 
 * This file holds the main experiment code.
 * 
 * Requires:
 *   config.js
 *   psiturk.js
 *   utils.js
 */

// Create and initialize the experiment configuration object
var $c = new Config(condition, counterbalance);

// Initalize psiturk object
var psiTurk = new PsiTurk(uniqueId, adServerLoc);

// Preload the HTML template pages that we need for the experiment
psiTurk.preloadPages($c.pages);

// Objects to keep track of the current phase and state
var CURRENTVIEW;
var STATE;
var counter = 0;
// var counter = 32;

/*************************
 * INSTRUCTIONS         
 *************************/

 var Instructions = function() {
	
	$(".slide").hide();
	var slide = $("#instructions-training-1");
	slide.fadeIn($c.fade);

	slide.find('.next').click(function () {
		CURRENTVIEW = new TestPhase();
	});


};



/*****************
 *  TRIALS       *
 *****************/

var TestPhase = function() {
	/* Instance variables */
	
	// Information about the current trial
	this.scenario;    
	// The response they gave
	this.response;
	// The number they've gotten correct, so far
	this.num_correct = 0;

	// Initialize a new trial. This is called either at the beginning
	// of a new trial, or if the page is reloaded between trials.
	this.init_trial = function () {
		// debug("Initializing trial " + STATE.index);

		// If there are no more trials left, then we are at the end of
		// this phase
		if (counter >= $c.scenarios.length) { //change here for debugging
			this.finish();
			return false;
		}

		if (counter < 0) {
			this.scenario = $c.tests[counter + $c.tests.length];
			$("#progress").hide();

		} else {

			// Load the new trialinfo
			this.scenario = $c.scenarios[counter];

			$("#progress").show();

			// Update progress bar
			update_progress(counter, $c.scenarios.length);



		}


		return true;
	}; 

	this.display_stim = function (that) {
		if (that.init_trial()) {
			debug("Show STIMULUS");
			// Show stimuli

			//show games 
			var games = that.scenario.games;
			var colorsList = $c.colors;
			var choices = $c.choices;
			var playersList = [];

			for (var i = 0; i < games.length; i++) {
				for (var j = 0; j < games[i].team1.length; j++) {
					if (playersList.indexOf(games[i].team1[j]) < 0) {
						playersList.push(games[i].team1[j]);
					}
				}

				for (var j = 0; j < games[i].team2.length; j++) {
					if (playersList.indexOf(games[i].team2[j]) < 0) {
						playersList.push(games[i].team2[j]);
					}
				}
			}

			var colors = {};
			for (var i = 0; i < playersList.length; i++) {
				colors[playersList[i]] = colorsList[i];
			}

			//function to assign names and colors
			var nameify = function(str) {
				var newstr = str;
				for (var key in colors) {
					newstr = newstr.replace(' ' + key + ' ', " <span style='background-color:" + colors[key] + "' class='name-in-text'>" + key + "</span> ");
				}
				return newstr;
			};

			//show prompt 
			if (counter < 0) {
				$("#prompt-text").text("TEST QUESTIONS");
			} else {
				$("#prompt-text").html($c.prompt);
			}
	
			var html = "";
			for (var i = 0; i < games.length; i++) {
				var team1 = games[i].team1;
				var team2 = games[i].team2;
				var winner = games[i].winner;

				html += "<h3>Game " + (i+1) + "</h3><ul class='game'>";
				if (winner == 1) {
					html += "<li><img src='static/images/crown.png' alt='crown'/></li>";
				} else {
					html += "<li><img src='static/images/nocrown.png' alt='crown'/></li>";
				}
				html += "<li><ul class='team' style='border-color:blue'>";
				for (var j = 0; j < team1.length; j++) {
					html += "<li style='background-color:" + colors[team1[j]] + "'>"+team1[j]+"</li>";
				}
				html += "</ul></li><li> VS. </li><li><ul class='team' style='border-color:red'>";
				for (var j = 0; j < team2.length; j++) {
					html += "<li style='background-color:" + colors[team2[j]] + "'>"+team2[j]+"</li>";
				}
				html += "</ul></li>"
				if (winner == 2) {
					html += "<li><img src='static/images/crown.png' alt='crown'/></li>";
				} else {
					html += "<li><img src='static/images/nocrown.png' alt='crown'/></li>";
				}

				html += "</ul>";

				if (i != games.length - 1) {
					html += "<hr style='width:75%'/>";
				}
			}

			$('#games').html(html);

			//show comments
			html = "";
			var comments = that.scenario.comments;
			for (var i = 0; i < comments.length; i++) {
				html += "<p>" + nameify(comments[i]) + "</p>";
			}

			$('#commentary').html(html);

			if (comments.length > 0) {
				$(".right_table").show();
				$(".left_table").css("width", "60%");
			} else {
				$(".right_table").hide();
				$(".left_table").css("width", "100%");
			}
			
			if (counter >= 0) {

				html = "<h4>Please answer the following question:</h4>";
				var scenarioQuestions = that.scenario.questions
				var scenarioSubjects = that.scenario.subjects;
				for (var i = 0; i < scenarioQuestions.length; i++) {
					var view = scenarioSubjects[i];
					var q = (Mustache.render($c.questions[scenarioQuestions[i]], view));
					html += '<p class="question">' + nameify(q) +'</p><div class="s-'+i+'"></div><div class="l-'+i+'"></div><br />' ;
				}


				$('#questions').html(html);

				// Build the sliders for each question
				for (var i=0; i<scenarioQuestions.length; i++) {
					// Create the sliders
					$('.s-'+i).slider().on("slidestart", function( event, ui ) {
						// Show the handle
						$(this).find('.ui-slider-handle').show() ;

						// Sum is the number of sliders that have been clicked
						var sum = 0 ;
						for (var j=0; j<scenarioQuestions.length; j++) {
							if ($('.s-'+j).find('.ui-slider-handle').is(":visible")) {
								sum++ ;
							}
						}
						// If the number of sliders clicked is equal to the number of sliders
						// the user can continue. 
						if (sum == scenarioQuestions.length) {
							$('#trial_next').prop('disabled', false) ;
						}
					});

					// Put labels on the sliders
					$('.l-'+i).append("<label style='width: 33%'><i>" + choices[scenarioQuestions[i]].low + "</i></label>") ; 
					// $('.l-'+i).append("<label style='width: 33%'><i>Average Strength</i></label>") ; 
					$('.l-'+i).append("<label style='width: 33%'><i></i></label>") ; 
					$('.l-'+i).append("<label style='width: 33%'><i>" + choices[scenarioQuestions[i]].high + "</i></label>");
										   
				}

			} else {

				html = "<h4>Please answer the following question about the tournament shown above:</h4>";
				if (counter == -2) {
					html += '<p class="question">Who won the above game?</p>';
					html += '<ul style="list-style-type:none; padding:0"><li><button id="t_buttonRick">Rick</button></li><li><button id="t_buttonAndy">Andy</button></li></ul>';
				} else if (counter == -1) {
					html += '<p class="question">Which of the following scenarios is the only one possible?</p>';
					html += '<ul style="list-style-type:none; padding:0"><li><button id="t_stronger">Hubert was stronger in Game 3 than in Game 2.</button></li><li><button id="t_lazy">Hubert did not try hard in Game 2, but he did try hard in Game 3.</button></li><li><button id="t_lazy_strong">David is stronger than George and David tried hard in Game 1.</button></li></ul>';
				}

				$('#questions').html(html);
				$('#questions').css("text-align", "center");

			}	


			$("#t_buttonRick").click(function() {
				$("#dialog").dialog("open");
				counter = -$c.tests.length;
				CURRENTVIEW = new Instructions();
			});
			$("#t_buttonAndy").click(function() {
				counter = counter + 1;
				CURRENTVIEW = new TestPhase();
			});

			$("#t_lazy_strong, #t_stronger").click(function() {
				$("#dialog").dialog("open");
				counter = -$c.tests.length;
				CURRENTVIEW = new Instructions();
			});
			$("#t_lazy").click(function() {
				$("#dialog").text("You have passed the test questions. The experiment will now begin.");
				$("#dialog").dialog("open");
				counter = 0;
				CURRENTVIEW = new TestPhase();
			});


			$(".team li, .name-in-text").hover(function() {
				var that = $(this);
				$(".team li, .name-in-text").each(function() {
					if ($(this).text() == that.text()) {
						$(this).css({
							"border": "2px solid black",
							"font-weight": "bold"
						});
					}
				});
			}, function() {
				var that = $(this);
				$(".team li, .name-in-text").each(function() {
					if ($(this).text() == that.text()) {
						$(this).css({
							"border": "2px solid white",
							"font-weight": "normal"
						});
					}
				});
			});


			$(".team li, .name-in-text").click(function(e) {
				e.stopPropagation();
				var pos = $(this).position();
				var name = $(this).text();
				var color = colors[name];
				var gamesIn = [];

				var popup = $(".player-popup");
				popup.hide();
				popup.css({
					"top": pos.top + "px",
					"left": pos.left + "px",
					"background-color": color
				});

				for (var i = 0; i < games.length; i++) {
					if (games[i].team1.indexOf(name) > -1 || games[i].team2.indexOf(name) > -1) {
						gamesIn.push(i + 1);
					}
				}
				var bHtml = "<p><strong>" + name + "</strong></p><p><i>was in:</i></p>";
				for (var i = 0; i < gamesIn.length; i++) {
					bHtml += "<p>Game " + gamesIn[i] + "</p>";
				}
				popup.html(bHtml);
				popup.slideDown(150);
			});

			$(document).click(function() {
				$(".player-popup").hide();
			});

			// Hide all the slider handles 
			$('.ui-slider-handle').hide() ;

			if (counter < 0) {
				$("#trial_next").hide();
			} else {
				$("#trial_next").show();
			}

			// Disable button which will be enabled once the sliders are clicked
			$('#trial_next').prop('disabled', true);

			$(document).scrollTop(0);
		}        
	};


	//records response 
	this.record_response = function() {        
		// for (var i=0; i<this.scenario.questions.length; i++) {
		// 	response.push($('.s-'+i).slider('value'));  
		// }

		var response = $('.s-0').slider('value');
		var trial = this.scenario.id;
		var data = {
			response: response,
			trial: trial
		}
		
		psiTurk.recordTrialData(data)
		counter += 1;
		
		// Update the page with the current phase/trial
		this.display_stim(this);
	};

	 this.finish = function() {
		debug("Finish test phase");

		// Change the page
		CURRENTVIEW = new Demographics()
	};

	// Load the trial html page
	$(".slide").hide();
	$(".player-popup").hide();

	// Show the slide
	var that = this; 
	$("#trial").fadeIn($c.fade);

	$(function() {
		$( "#dialog" ).dialog({
		  autoOpen : false,
		  resizable: false,
		  height: "auto",
		  width: 400,
		  modal: true,
		  buttons: {
			"OK": function() {
			  $( this ).dialog( "close" );
			}
		  }
		});
	})


	$('#trial_next').unbind();
	$('#trial_next').click(function (e) {
		that.record_response();
	});


	// Initialize the current trial
	if (this.init_trial()) {
		// Start the test
		this.display_stim(this);
	};
};

/*****************
 *  DEMOGRAPHICS*
 *****************/

var Demographics = function(){

	var that = this; 

// Show the slide
$(".slide").hide();
$("#demographics").fadeIn($c.fade);

	//disable button initially
	$('#trial_finish').prop('disabled', true);

	//checks whether all questions were answered
	$('.demoQ').change(function () {
	   if ($('input[name=sex]:checked').length > 0 &&
		 $('input[name=age]').val() != "")
	   {
		$('#trial_finish').prop('disabled', false)
	}else{
		$('#trial_finish').prop('disabled', true)
	}
});

// deletes additional values in the number fields 
$('.numberQ').change(function (e) {    
	if($(e.target).val() > 100){
		$(e.target).val(100)
	}
});

this.finish = function() {
	debug("Finish test phase");

		// Show a page saying that the HIT is resubmitting, and
		// show the error page again if it times out or error
		var resubmit = function() {
			$(".slide").hide();
			$("#resubmit_slide").fadeIn($c.fade);

			var reprompt = setTimeout(prompt_resubmit, 10000);
			psiTurk.saveData({
				success: function() {
					clearInterval(reprompt); 
					finish();
				}, 
				error: prompt_resubmit
			});
		};

		// Prompt them to resubmit the HIT, because it failed the first time
		var prompt_resubmit = function() {
			$("#resubmit_slide").click(resubmit);
			$(".slide").hide();
			$("#submit_error_slide").fadeIn($c.fade);
		};

		// Render a page saying it's submitting
		psiTurk.showPage("submit.html") ;
		psiTurk.saveData({
			success: psiTurk.completeHIT, 
			error: prompt_resubmit
		});
	}; //this.finish function end 

	$('#trial_finish').click(function () {           
	   var feedback = $('textarea[name = feedback]').val();
	   var sex = $('input[name=sex]:checked').val();
	   var age = $('input[name=age]').val();

	   psiTurk.recordUnstructuredData('feedback',feedback);
	   psiTurk.recordUnstructuredData('sex',sex);
	   psiTurk.recordUnstructuredData('age',age);
	   that.finish();
   });
};

// --------------------------------------------------------------------
// --------------------------------------------------------------------

/*******************
 * Run Task
 ******************/

$(document).ready(function() { 
	// Load the HTML for the trials
	psiTurk.showPage("trial.html");

	// Record various unstructured data
	psiTurk.recordUnstructuredData("condition", condition);
	psiTurk.recordUnstructuredData("counterbalance", counterbalance);

	// Start the experiment
	STATE = new State();
	counter = -2;
	// Begin the experiment phase
	if (STATE.instructions) {
		CURRENTVIEW = new Instructions();
	} else {
		CURRENTVIEW = new TestPhase();
	}
});
