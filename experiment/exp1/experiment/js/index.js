var order = 1;

exposure_stimuli = all_stimuli.filter(function (e){
  return e.list == 1;
});
test_stimuli = all_stimuli.filter(function (e){
  return e.list == 2;
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

exp_cond = shuffle(["CNPC","SUBJ","WH"])[0];
test_match_cond = shuffle(["match", "mismatch"])[0];


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
block_8 = exposure_stimuli.filter(function (e) {
  return e.block == 8;
});
block_9 = exposure_stimuli.filter(function (e) {
  return e.block == 9;
});
block_10 = exposure_stimuli.filter(function (e) {
  return e.block == 10;
});
block_11 = exposure_stimuli.filter(function (e) {
  return e.block == 11;
});
block_12 = exposure_stimuli.filter(function (e) {
  return e.block == 12;
});
block_13 = exposure_stimuli.filter(function (e) {
  return e.block == 13;
});
block_14 = exposure_stimuli.filter(function (e) {
  return e.block == 14;
});
block_15 = exposure_stimuli.filter(function (e) {
  return e.block == 15;
});

test_1 = test_stimuli.filter(function (e) {
  return e.block == 1;
});
test_2 = test_stimuli.filter(function (e) {
  return e.block == 2;
});
test_3 = test_stimuli.filter(function (e) {
  return e.block == 3;
});
test_4 = test_stimuli.filter(function (e) {
  return e.block == 4;
});
test_5 = test_stimuli.filter(function (e) {
  return e.block == 5;
});
test_6 = test_stimuli.filter(function (e) {
  return e.block == 6;
});



block_1 = shuffle(block_1);
block_2 = shuffle(block_2);
block_3 = shuffle(block_3);
block_4 = shuffle(block_4);
block_5 = shuffle(block_5);
block_6 = shuffle(block_6);
block_7 = shuffle(block_7);
block_8 = shuffle(block_8);
block_9 = shuffle(block_9);
block_10 = shuffle(block_10);
block_11 = shuffle(block_11);
block_12 = shuffle(block_12);
block_13 = shuffle(block_13);
block_14 = shuffle(block_14);
block_15 = shuffle(block_15);
test1 = shuffle(test_1);
test2 = shuffle(test_2);
test3 = shuffle(test_3);
test4 = shuffle(test_4);
test5 = shuffle(test_5);
test6 = shuffle(test_6);

shuffled_blocks = shuffle([block_1, block_2, block_3, block_4, block_5, block_6, block_7, block_8, block_9, block_10, block_11, block_12, block_13, block_14, block_15]);
shuffled_tests = shuffle([test_1, test_2, test_3, test_4, test_5, test_6]);

//uncomment to shorten experiment for debugging
//shuffled_blocks = shuffle([block_1]);
name_list = shuffle(["Gregory", "Emily", "Jessy", "Thomas"]);

for (var i = 0; i < 15; i++){
  for (var j in shuffled_blocks[i]){

    shuffled_blocks[i][j]["new_block_sequence"] = i+1;
    shuffled_blocks[i][j]["island_tested"] = exp_cond;
    shuffled_blocks[i][j]["phase"] = "exposure"
    shuffled_blocks[i][j]["test_match_cond"] = test_match_cond;
    if (shuffled_blocks[i][j]["condition"] == exp_cond){
        shuffled_blocks[i][j]["name"] = name_list[0];}
    else if (shuffled_blocks[i][j]["condition"] == "FILL"){
        shuffled_blocks[i][j]["name"] = name_list[1]}
    else if (shuffled_blocks[i][j]["condition"] == "UNGRAM"){
        shuffled_blocks[i][j]["name"] = "Iron-Head";}
    else {
      shuffled_blocks[i][j]["name"] = "delete";}
      
  };
};

for (var i = 0; i < 6; i++){
  for (var j in shuffled_tests[i]){

    shuffled_tests[i][j]["new_block_sequence"] = i+101;
    shuffled_tests[i][j]["phase"] = "test"
    shuffled_tests[i][j]["island_tested"] = exp_cond;
    shuffled_tests[i][j]["test_match_cond"] = test_match_cond;
      if (test_match_cond == "mismatch"){
          
        if (shuffled_tests[i][j]["condition"] == exp_cond){
            shuffled_tests[i][j]["name"] = name_list[1];}
        else if (shuffled_tests[i][j]["condition"] == "FILL"){
            shuffled_tests[i][j]["name"] = name_list[0]}
        else if (shuffled_tests[i][j]["condition"] == "UNGRAM"){
            shuffled_tests[i][j]["name"] = "Iron-Head";}
        else {
          shuffled_tests[i][j]["name"] = "delete";}
          
      } 
      else {
      
        if (shuffled_tests[i][j]["condition"] == exp_cond){
          shuffled_tests[i][j]["name"] = name_list[0];}
      else if (shuffled_tests[i][j]["condition"] == "FILL"){
          shuffled_tests[i][j]["name"] = name_list[1]}
      else if (shuffled_tests[i][j]["condition"] == "UNGRAM"){
          shuffled_tests[i][j]["name"] = "Iron-Head";}
      else {
        shuffled_tests[i][j]["name"] = "delete";}


      };

    };
};

latin_squared = [shuffled_blocks, shuffled_tests].flat().flat();
console.log(latin_squared)
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

  // from the old version with characters:

  // slides.practice_slider = slide({
  //   name : "practice_slider",

  //   /* trial information for this block
  //    (the variable 'stim' will change between each of these values,
  //     and for each of these, present_handle will be run.) */
  //   present : [{"a": 1}],
  //   //this gets run only at the beginning of the block
  //   present_handle : function(stim) {
  //     var name = 'John';
  //     $(".err").hide();
  //     $(".errgood").hide();
  //     $(".errbad").hide();
  //     $(".target").hide();
  //     $(".slider_table").hide();
  //     $(".button_2").hide()
  //     this.stim = stim;
    
  //     // var init_image = '<img src="images/'+ name + '.png" style="height:150px" class="center">';
  //     // $(".image").html(init_image)
  //     // $(".figure_intro").html("This is <b>"+name+"<\/b.")
  //     $(".context").html("Context: The boy saw an apple on the table.");
  //     $(".target").html(name " asks: <b> What did the boy see on the table? <\/b>");
  //     $(".button_1").html("Click here to see what <b>" +name+ "<\/b> asks about the context.")
  //     this.init_sliders();
  //     exp.sliderPost = null; //erase current slider value
  //     exp.first_response_wrong = 0;
  //     exp.first_response_value = null;
  //     exp.attempts = 0;
  //   },
  //   button_1 : function() {
  //     $(".target").show();
  //     $(".slider_table").show();
  //     $(".button_2").show()
  //     $(".button_1").hide()
  //   },
  //   button_2 : function() {
 
  //     if (exp.sliderPost == null) {
  //       $(".err").show();

  //     } 
  //     else if (exp.sliderPost < 0.5) {
  //       exp.first_response_wrong = 1;
  //       exp.first_response_value =exp.sliderPost;
  //       exp.attempts = exp.attempts + 1;
  //       $(".errgood").show();
  //     }
  //     else {
  //       this.log_responses();
  //       /* use _stream.apply(this); if and only if there is
  //       "present" data. (and only *after* responses are logged) */
  //       _stream.apply(this);
  //     }
  //   },
  //   init_sliders : function() {
  //     utils.make_slider("#practice_slider_1", function(event, ui) {
  //       exp.sliderPost = ui.value;
  //     });
  //   },
  //   log_responses : function() {
  //     exp.data_trials.push({
  //       "response" : exp.sliderPost,
  //       "first_response_value": exp.first_response_value,
  //       "wrong_attempts": exp.attempts,
  //       "condition" : "practice_good",
  //       "block_sequence": "practice",
  //       "item_number": "practice_good",
  //       "list_number": "practice",
  //       "trial_sequence_total": 0
  //     });

  //   }
  // });

  slides.post_practice_1 = slide({
    name : "post_practice_1",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });


  // from the old version with characters:

  // slides.practice_slider_bad = slide({
  //   name : "practice_slider_bad",

  //   /* trial information for this block
  //    (the variable 'stim' will change between each of these values,
  //     and for each of these, present_handle will be run.) */
  //   present : [1],

  
  //   //this gets run only at the beginning of the block
  //   present_handle : function(stim) {
  //     var name = 'Mary';
  //     $(".button_1").show()
  //     $(".err").hide();
  //     $(".errgood").hide();
  //     $(".errbad").hide();
  //     $(".target").hide();
  //     $(".slider_table").hide();
  //     $(".button_2").hide()
  //     this.stim = stim;
    
  //     // var init_image = '<img src="images/'+ name + '.png" style="height:150px" class="center">';
  //     // $(".image").html(init_image)
  //     // $(".figure_intro").html("This is <b>"+name+"<\/b.")
  //     $(".context").html("Context: The girl slept under the bed.");
  //     $(".target").html(name + " asks: <b> Who the bed was slept under? <\/b>")
  //     $(".button_1").html("Click here to see what <b>" +name+ "<\/b> asks about the context.")
  //     this.init_sliders();
  //     exp.sliderPost = null; //erase current slider value
  //     exp.first_response_wrong = 0;
  //     exp.first_response_value = null;
  //     exp.attempts = 0;
  //   },
  //   button_1 : function() {
  //     $(".target").show();
  //     $(".slider_table").show();
  //     $(".button_2").show()
  //     $(".button_1").hide()
  //   },
  //   button_2 : function() {
 
  //     if (exp.sliderPost == null) {
  //       $(".err").show();
        
  //     } 
  //     else if (exp.sliderPost > 0.5) {
  //       exp.first_response_wrong = 1;
  //       exp.first_response_value = exp.sliderPost;
  //       exp.attempts = exp.attempts + 1;
  //       $(".errbad").show();
  //     }
  //     else {
  //       this.log_responses();
  //       /* use _stream.apply(this); if and only if there is
  //       "present" data. (and only *after* responses are logged) */
  //       _stream.apply(this);
  //     }
  //   },
  //   init_sliders : function() {
  //     utils.make_slider("#practice_slider_2", function(event, ui) {
  //       exp.sliderPost = ui.value;
        
  //     });
  //   },
  //   log_responses : function() {
  //     exp.data_trials.push({
  //       "response" : exp.sliderPost,
  //       "first_response_value": exp.first_response_value,
  //       "wrong_attempts": exp.attempts,
  //       "condition" : "practice_bad",
  //       "block_sequence": "practice",
  //       "item_number": "practice_bad",
  //       "list_number": "practice",
  //       "trial_sequence_total": 0
  //     });

  //   }
  // });

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

  slides.speaker_intro1 = slide({
    name : "speaker_intro1",
    //this gets run only at the beginning of the block
    start : function(){
      var init_image = '<img src="images/'+ name_list[0] + '.png" style="height:150px" class="center">';

      $(".speaker_intro_line").html("Let me introduce you to "+name_list[0]+", "+name_list[1]+", and Iron-Head! They will be asking some questions during this experiment, and you will be rating how acceptable their questions sound!")
      $(".speaker_image").html(init_image)
      $(".specific_speaker_intro_line").html("This is "+name_list[0]+"!")
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  slides.speaker_intro2 = slide({
    name : "speaker_intro2",
    //this gets run only at the beginning of the block
    start : function(){
      var init_image = '<img src="images/'+ name_list[1] + '.png" style="height:150px" class="center">';

      $(".speaker_intro_line").html("Let me introduce you to "+name_list[0]+", "+name_list[1]+", and Iron-Head! They will be asking some questions during this experiment, and you will be rating how acceptable their questions sound!")
      $(".speaker_image").html(init_image)
      $(".specific_speaker_intro_line").html("This is "+name_list[1]+"!")
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  slides.speaker_intro5 = slide({
    name : "speaker_intro5",
    //this gets run only at the beginning of the block
    start : function(){
      var init_image = '<img src="images/Iron-Head.png" style="height:150px" class="center">';

      $(".speaker_intro_line").html("Let me introduce you to "+name_list[0]+", "+name_list[1]+", and Iron-Head! They will be asking some questions during this experiment, and you will be rating how acceptable their questions sound!")
      $(".speaker_image").html(init_image)
      $(".specific_speaker_intro_line").html("This is <b>Iron-Head<\/b>! Iron-Head is a robot!")
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }

  });

  
  slides.speaker_intro_final = slide({
    name : "speaker_intro_final",
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

  // slides.one_slider = slide({
  //   name : "one_slider",

  //   /* trial information for this block
  //    (the variable 'stim' will change between each of these values,
  //     and for each of these, present_handle will be run.) */
  //   present : latin_squared,
    
    
  //   //this gets run only at the beginning of the block
  //   present_handle : function(stim) {
  //     $(".err").hide();
  //     $(".target").hide();
  //     $(".slider_table").hide();
  //     $(".button_2").hide()
  //     $(".button_3").hide()
  //     $(".speaker_question").hide()
  //     $(".context").show();
  //     $(".button_1").show();
  //     $(".image").show();
  //     $(".figure_intro").show();
  //     comp_question_exist = 0;
  
  //     this.stim = stim; //I like to store this information in the slide so I can record it later.
  //     $(".context").html(stim.presented_context);
  //     var init_image = '<img src="images/'+ stim.name + '.png" style="height:150px" class="center">';
  //     $(".image").html(init_image)
  //     $(".button_1").html("Click here to see what <b>"+ stim.name + "<\/b> asks about the context.")
  //     $(".figure_intro").html("This is <b>"+stim.name+"<\/b.")
  //     $(".target").html(stim.name + " asks: " + stim.presented_target);

  //     this.init_sliders()
  //     exp.sliderPost = null; //erase current slider value
  //     resetSelectElement(comp_q);
  //   },
  //   button_1 : function() {
  //     $(".target").show();
  //     $(".button_1").hide();
  //     $(".slider_table").show();
  //     if (Math.random() > 0.75){
  //       comp_question_exist = 1
  //       $(".button_3").show()
  //     }
  //     else{
  //     comp_question_exist = 0
  //     $(".button_2").show()
  //     }
  //   },

  //   button_3 : function() {
  //     if (exp.sliderPost == null) {
  //       $(".err").show();
  //     }
  //     else {
  //     $(".target").hide();
  //     $(".context").hide();
  //     $(".slider_table").hide();
  //     $(".button_1").hide();
  //     $(".image").hide();
  //     $(".figure_intro").hide()
  //     $(".speaker_question").show();
  //     $(".button_3").hide()
  //     $(".button_2").show()
  //     }
  //   },

  //   button_2 : function() {
  //     if (exp.sliderPost == null) {
  //       $(".err").show();
  //     }
  //     else {
  //       this.log_responses();
  //       /* use _stream.apply(this); if and only if there is
  //       "present" data. (and only *after* responses are logged) */
  //       _stream.apply(this);
  //     }
    
  // },
    
  //   init_sliders : function() {
  //     utils.make_slider("#single_slider", function(event, ui) {
  //       exp.sliderPost = ui.value;
  //     });
  //   },

  //   log_responses : function() {
  //     exp.data_trials.push({
  //       "response" : exp.sliderPost,
  //       "island_tested": this.stim.island_tested,
  //       "test_match_cond":this.stim.test_match_cond,
  //       "condition" : this.stim.condition,
  //       "block_sequence": this.stim.new_block_sequence,
  //       "item_number": this.stim.item,
  //       "list_number": this.stim.list,
  //       "trial_sequence_total": order,
  //       "speaker_identity":this.stim.name,
  //       "comp_question_exist": comp_question_exist,
  //       "comp_answer": $("#comp_q").val(),
  //       "phase": this.stim.phase
  //     });
  //     order = order + 1;
  //   }
  // });

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
        "block_sequence": this.stim.new_block_sequence,
        "item_number": this.stim.item,
        "list_number": this.stim.list,
        "trial_sequence_total": order,
        "phase": this.stim.phase
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
