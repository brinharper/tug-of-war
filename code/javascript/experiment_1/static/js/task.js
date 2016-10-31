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
		debug("Initializing trial " + STATE.index);

		// If there are no more trials left, then we are at the end of
		// this phase
		if (STATE.index >= $c.scenarios.length) { //change here for debugging
			this.finish();
			return false;
		}

		
		// Load the new trialinfo
		this.scenario = $c.scenarios[STATE.index];

		// Update progress bar
		update_progress(STATE.index, $c.scenarios.length);

		return true;
	}; 

	this.display_stim = function (that) {
		if (that.init_trial()) {
			debug("Show STIMULUS");
			// Show stimuli

			//show games 
			var games = that.scenario.games;
			var html = "";
			for (var i = 0; i < games.length; i++) {
				var team1 = games[i].team1;
				var team2 = games[i].team2;
				var winner = games[i].winner;

				html += "<ul class='game'>";
				if (winner == 1) {
					html += "<li><img src='static/images/crown.png' alt='crown'/></li>";
				} else {
					html += "<li><img src='static/images/nocrown.png' alt='crown'/></li>";
				}
				html += "<li><ul class='team' style='border-color:blue'>";
				for (var j = 0; j < team1.length; j++) {
					html += "<li>"+team1[j]+"</li>";
				}
				html += "</ul></li><li> VS. </li><li><ul class='team' style='border-color:red'>";
				for (var j = 0; j < team2.length; j++) {
					html += "<li>"+team2[j]+"</li>";
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
				html += "<p>" + comments[i] + "</p>";
			}

			$('#commentary').html(html);
			

			html = "<p><b>Please rate how strongly you agree with each of the following statements:</b></p>";
			var scenarioQuestions = that.scenario.questions
			var scenarioSubjects = that.scenario.subjects;
			var view = {"player": ""};
			for (var i = 0; i < scenarioQuestions.length; i++) {
				view["player"] = scenarioSubjects[i];
				var q = (Mustache.render($c.questions[scenarioQuestions[i]], view));
				html += '<p class=".question">' + q +'</p><div class="s-'+i+'"></div><div class="l-'+i+'"></div><br />' ;
			}

			$('#questions').html(html) ;

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
				$('.l-'+i).append("<label style='width: 33%'><i>Disagree Completely</i></label>") ; 
				$('.l-'+i).append("<label style='width: 33%'></label>") ; 
				$('.l-'+i).append("<label style='width: 33%'><i>Agree Completely</i></label>");
									   
			}

			// Hide all the slider handles 
			$('.ui-slider-handle').hide() ;

			// Disable button which will be enabled once the sliders are clicked
			$('#trial_next').prop('disabled', true);


			debug(that.trialinfo);
		}        
	};

	//records response 
	this.record_response = function() {        
		
		var response = [] ;
		saveid = this.scenario.id;
		   
		for (var i=0; i<this.scenario.questions.length; i++) {
			response.push($('.s-'+i).slider('value'));  
		}
		debug(response)
		psiTurk.recordTrialData(["trial_".concat(saveid), "judgments", response]);


		STATE.set_index(STATE.index + 1);
		
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

	// Show the slide
	var that = this; 
	$("#trial").fadeIn($c.fade);
	$('#trial_next.next').click(function () {
		that.record_response();
	});


	// Initialize the current trial
	if (this.init_trial()) {
		// Start the test
		this.display_stim(this) ;
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
	// Begin the experiment phase
	if (STATE.instructions) {
		CURRENTVIEW = new Instructions();
	} else {
		CURRENTVIEW = new TestPhase();
	}
});
