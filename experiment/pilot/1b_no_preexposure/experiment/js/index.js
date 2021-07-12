var order = 1;

exposure_stimuli = all_stimuli.filter(function (e){
  return e.list == 1;
});
test_stimuli = all_stimuli.filter(function (e){
  return e.list == 2;
});
control_exposure_stimuli = all_stimuli.filter(function (e){
  return e.list == 3;
});

var shuffle = function (array) {

	var currentIndex = array.length;
	var temporaryValue, randomIndex;

	// While there remain elements to shuffle...
	while (0 !== currentIndex) {
		// Pick a remaining element...
		randomIndex = Math.floor(Math.random() * currentIndex);
		currentIndex -= 1;

		// And swap it with the current element.
		temporaryValue = array[currentIndex];
		array[currentIndex] = array[randomIndex];
		array[randomIndex] = temporaryValue;
	}

	return array;

};


// choose experimental condition
shuffled_conditions = shuffle(["CNPC","SUBJ","WH"]);
exp_cond = shuffled_conditions[0];
test_cond = shuffled_conditions[0];


// for pilot, we're just doing whether and subject
exp_cond = "WH";
test_cond = "SUBJ";

// console.log("exposure condition: ", exp_cond);
// console.log("test condition: ", test_cond);

// choose an experimental group
// Control sees the same condition throughout
// Exposure sees different conditions on exposure and test
experiment_group = shuffle(["CNTRL", "EXP"])[0];

//console.log(experiment_group);




// 7 blocks = 14 stimuli, 14 fillers
block_1 = exposure_stimuli.filter(function (e) {
  return e.block == 1;
});
block_2 = exposure_stimuli.filter(function (e) {
  return e.block == 2;
});
block_3 = exposure_stimuli.filter(function (e) {
  return e.block == 3;
});
block_4 = exposure_stimuli.filter(function (e) {
  return e.block == 4;
});
block_5 = exposure_stimuli.filter(function (e) {
  return e.block == 5;
});
block_6 = exposure_stimuli.filter(function (e) {
  return e.block == 6;
});
block_7 = exposure_stimuli.filter(function (e) {
  return e.block == 7;
});

// 3 blocks = 6 stimuli, 6 fillers
// the same in either experiment group
test_1 = test_stimuli.filter(function (e) {
  return e.block == 1;
});
test_2 = test_stimuli.filter(function (e) {
  return e.block == 2;
});
test_3 = test_stimuli.filter(function (e) {
  return e.block == 3;
});



// 7 blocks = 14 stimuli (same condition as test phase items), 14 fillers
control_exp_1 = control_exposure_stimuli.filter(function (e) {
  return e.block == 1;
});
control_exp_2 = control_exposure_stimuli.filter(function (e) {
  return e.block == 2;
});
control_exp_3 = control_exposure_stimuli.filter(function (e) {
  return e.block == 3;
});
control_exp_4 = control_exposure_stimuli.filter(function (e) {
  return e.block == 4;
});
control_exp_5 = control_exposure_stimuli.filter(function (e) {
  return e.block == 5;
});
control_exp_6 = control_exposure_stimuli.filter(function (e) {
  return e.block == 6;
});
control_exp_7 = control_exposure_stimuli.filter(function (e) {
  return e.block == 7;
});


block_1 = shuffle(block_1);
block_2 = shuffle(block_2);
block_3 = shuffle(block_3);
block_4 = shuffle(block_4);
block_5 = shuffle(block_5);
block_6 = shuffle(block_6);
block_7 = shuffle(block_7);
// block_8 = shuffle(block_8);
// block_9 = shuffle(block_9);
// block_10 = shuffle(block_10);
// block_11 = shuffle(block_11);
// block_12 = shuffle(block_12);
// block_13 = shuffle(block_13);
// block_14 = shuffle(block_14);
// block_15 = shuffle(block_15);
test_1 = shuffle(test_1);
test_2 = shuffle(test_2);
test_3 = shuffle(test_3);

control_exp_1 = shuffle(control_exp_1);
control_exp_2 = shuffle(control_exp_2);
control_exp_3 = shuffle(control_exp_3);
control_exp_4 = shuffle(control_exp_4);
control_exp_5 = shuffle(control_exp_5);
control_exp_6 = shuffle(control_exp_6);
control_exp_7 = shuffle(control_exp_7);

shuffled_exp = shuffle([block_1, block_2, block_3, block_4, block_5, block_6, block_7]);
shuffled_tests = shuffle([test_1, test_2, test_3]);
shuffled_control_exp = shuffle([control_exp_1, control_exp_2, control_exp_3, control_exp_4, control_exp_5, control_exp_6, control_exp_7]);

//uncomment to shorten experiment for debugging
// shuffled_exp = shuffle([block_1]);
// shuffled_tests = shuffle([test_1]);
// shuffled_control_exp = shuffle([control_exp_1]);

var num_exp_blocks = 7;
var num_test_blocks = 3;
var num_control_exp_blocks = 7;

for(var i = 0; i < num_exp_blocks; i++){
  for (var j in shuffled_exp[i]){

    shuffled_exp[i][j]["new_block_sequence"] = i+1;
    shuffled_exp[i][j]["exposure_condition"] = exp_cond;
    shuffled_exp[i][j]["test_condition"] = test_cond;
    shuffled_exp[i][j]["phase"] = "exposure";
    shuffled_exp[i][j]["group"] = experiment_group;

    // tag sentences to be filtered out (not in the exposure condition)
    if (shuffled_exp[i][j]["condition"] == exp_cond) {
      shuffled_exp[i][j]["name"] = "good";
    }
    else if (shuffled_exp[i][j]["condition"] == "FILL") {
      shuffled_exp[i][j]["name"] = "good"
    }
    else if (shuffled_exp[i][j]["condition"] == "UNGRAM") {
      shuffled_exp[i][j]["name"] = "good";
    }
    else {
      shuffled_exp[i][j]["name"] = "delete";
    }


      
  };
};

for (var i = 0; i < num_test_blocks; i++){
  for (var j in shuffled_tests[i]){

    shuffled_tests[i][j]["new_block_sequence"] = i+101;
    shuffled_tests[i][j]["phase"] = "test";
    if(experiment_group == "EXP")
      shuffled_tests[i][j]["exposure_condition"] = exp_cond;
    else
      shuffled_tests[i][j]["exposure_condition"] = test_cond;
    shuffled_tests[i][j]["test_condition"] = test_cond;
    shuffled_tests[i][j]["group"] = experiment_group;
    
    // tag sentences to be filtered out (not in the test condition)
    if (shuffled_tests[i][j]["condition"] == test_cond){
        shuffled_tests[i][j]["name"] = "good";}
    else if (shuffled_tests[i][j]["condition"] == "FILL"){
        shuffled_tests[i][j]["name"] = "good"}
    else if (shuffled_tests[i][j]["condition"] == "UNGRAM"){
        shuffled_tests[i][j]["name"] = "good";}
    else {
      shuffled_tests[i][j]["name"] = "delete";}

    };
};

for (var i = 0; i < num_control_exp_blocks; i++){
  for (var j in shuffled_control_exp[i]){

    shuffled_control_exp[i][j]["new_block_sequence"] = i+1;
    shuffled_control_exp[i][j]["phase"] = "pre-exposure";
    shuffled_control_exp[i][j]["exposure_condition"] = test_cond;
    shuffled_control_exp[i][j]["test_condition"] = test_cond;
    shuffled_control_exp[i][j]["group"] = experiment_group;
   
    // tag sentences to be filtered out (not in the test condition)
    if (shuffled_control_exp[i][j]["condition"] == test_cond) {
      shuffled_control_exp[i][j]["name"] = "good";
    }
    else if (shuffled_control_exp[i][j]["condition"] == "FILL") {
      shuffled_control_exp[i][j]["name"] = "good"
    }
    else if (shuffled_control_exp[i][j]["condition"] == "UNGRAM") {
      shuffled_control_exp[i][j]["name"] = "good";
    }
    else {
      shuffled_control_exp[i][j]["name"] = "delete";
    }

    };
};

// create one list of all the items
if(experiment_group == "EXP"){
  latin_squared = [shuffled_exp, shuffled_tests].flat().flat();
}
else{
  latin_squared = [shuffled_control_exp, shuffled_tests].flat().flat();
}
// console.log(latin_squared);

// filter out items not in this condition
latin_squared = latin_squared.filter(i => i["name"] != "delete");

//shuffle name-condition coorelation

function resetSelectElement(selectElement) {
  var options = selectElement.options;

  // Look for a default selected option
  for (var i=0, iLen=options.length; i<iLen; i++) {

      if (options[i].defaultSelected) {
          selectElement.selectedIndex = i;
          return;
      }
  }
  // If no option is the default, select first or none as appropriate
  selectElement.selectedIndex = 0; // or -1 for no option selected
}


function make_slides(f) {
  var slides = {};  
  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.single_trial = slide({
    name: "single_trial",
    start: function() {
      $(".err").hide();
      $(".display_condition").html("You are in " + exp.condition + ".");
    },
    button : function() {
      response = $("#text_response").val();
      if (response.length == 0) {
        $(".err").show();
      } else {
        exp.data_trials.push({
          "trial_type" : "single_trial",
          "response" : response
        });
        exp.go(); //make sure this is at the *end*, after you log your data
      }
    },
  });

  slides.practice_slider = slide({
    name : "practice_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : [{"a": 1}],
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".errgood").hide();
      this.stim = stim;
      $(".prompt").html("Context: The boy saw an apple on the table. <p>  Target: <b> What did the boy see on the table? <\/b>");
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
      exp.first_response_wrong = 0;
      exp.first_response_value = null;
      exp.attempts = 0;
    },
    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } 
      else if (exp.sliderPost < 0.5) {
        exp.first_response_wrong = 1;
        exp.first_response_value =exp.sliderPost;
        exp.attempts = exp.attempts + 1;
        $(".errgood").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },
    init_sliders : function() {
      utils.make_slider("#practice_slider_1", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "first_response_value": exp.first_response_value,
        "wrong_attempts": exp.attempts,
        "condition" : "practice_good",
        "block_sequence": "practice",
        "item_number": "practice_good",
        "list_number": "practice",
        "trial_sequence_total": 0
      });

    }
  });


  slides.post_practice_1 = slide({
    name : "post_practice_1",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


 

  slides.practice_slider_bad = slide({
    name : "practice_slider_bad",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : [1],

  
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".errbad").hide();
      $(".prompt").html("Context: The girl slept under the bed. <p>  Target: <b> Who the bed was slept under? <\/b>");
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
      exp.first_response_wrong = 0;
      exp.first_response_value = null;
      exp.attempts = 0;
    },
    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } 
      else if (exp.sliderPost > 0.5) {
        exp.first_response_wrong = 1;
        exp.first_response_value = exp.sliderPost;
        exp.attempts = exp.attempts + 1;
        $(".errbad").show();
      }
      else {
        this.log_responses();
        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },
    init_sliders : function() {
      utils.make_slider("#practice_slider_2", function(event, ui) {
        exp.sliderPost = ui.value;
        
      });
    },
    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "first_response_value": exp.first_response_value,
        "wrong_attempts": exp.attempts,
        "condition" : "practice_bad",
        "block_sequence": "practice",
        "item_number": "practice_bad",
        "list_number": "practice",
        "trial_sequence_total": 0
      });

    }
  });

  slides.post_practice_2 = slide({
    name : "post_practice_2",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


  slides.last_reminder = slide({
    name : "last_reminder",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
    
  });

 

  slides.one_slider = slide({
    name : "one_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : latin_squared,
    
    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      this.stim = stim; //I like to store this information in the slide so I can record it later.
      $(".context").html(stim.presented_context);
      $(".target").html("Target: " + stim.presented_target);
      this.init_sliders()
      exp.sliderPost = null; //erase current slider value
    },

    button : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } else {
        this.log_responses();

        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },

    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
      exp.data_trials.push({
        "response" : exp.sliderPost,
        "condition" : this.stim.condition,
        "exposure_condition": this.stim.exposure_condition,
        "test_condition": this.stim.test_condition,
        "block_sequence": this.stim.new_block_sequence,
        "item_number": this.stim.item,
        "list_number": this.stim.list,
        "trial_sequence_total": order,
        "phase": this.stim.phase,
        "group": this.stim.group
      });
      order = order + 1;
    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];
  //exp.condition = _.sample(["condition 1", "condition 2"]); //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  // exp.structure=["i0", "instructions", "practice_slider", "post_practice_1", "practice_slider_bad", "post_practice_2", "speaker_intro1","speaker_intro2","speaker_intro5","speaker_intro_final", "last_reminder", 'one_slider', 'subj_info', 'thanks'];
  exp.structure=["i0", "instructions", "practice_slider", "post_practice_1", "practice_slider_bad", "post_practice_2", "last_reminder", 'one_slider', 'subj_info', 'thanks'];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
