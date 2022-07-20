var order = 1;

// I don't know why this exists, but it seems to work
// and we make use of it
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


num_target_items = 12;
num_gram_filler = 12;
num_ungram_filler = 12;
block_size = 3;

experiment_condition = shuffle(["WH", "CNPC"])[0];
experiment_group = shuffle(["RP", "GAP"])[0];


//console.log("condition", experiment_condition);
//console.log("group", experiment_group);

/**
 * 
 * @param {*} items 
 * @param {*} group filter 1
 * @param {*} cond  filter 2
 * @returns filtered items
 * 
 * Method Name: Filt
 * Purpose: filter an array with two filters
 * 
 */
function filt(items, cond){
    return items.filter(item => (item.condition == cond))
}

//Desired stimuli list
experiment_stimuli = filt(all_stimuli, experiment_condition);
//Desired grammatical filler
gram_filler = filt(all_stimuli, "FILL");
//Desired ungrammatical filler
ungram_filler = filt(all_stimuli, "UG");


//Testing logs
console.log("stimuli ", experiment_stimuli);
console.log("filler ", gram_filler);
console.log("ungram ", ungram_filler);


/**
 * Pseudo-block randomization of three lists of testing items
 * @param {*} list1 three lists of items
 * @param {*} list2 
 * @param {*} list3 
 * @returns An array of randomized blocks
 */
function pseudo_block(list1, list2, list3){
    output = [];
    //num = list1.length;
    num = 12;
    console.log('length', num)
    for(var i = 0; i < num; i++){
        block = [list1.pop(), list2.pop(), list3.pop()];
        block = shuffle(block);
        output.push(block);
    }
    return output;
}

//Create final blocks
final_stimuli_blocks = pseudo_block(experiment_stimuli, gram_filler, ungram_filler);
console.log("final stimuli", final_stimuli_blocks);

//tag information about the experiment conditions to each item
//not sure what should be tagged
for(var i = 0; i < 12; i++){
    for (var j = 0; j < block_size; j++){
  
        final_stimuli_blocks[i][j]["new_block_sequence"] = i + 1;
        final_stimuli_blocks[i][j]["exp_condition"] = experiment_condition;
        final_stimuli_blocks[i][j]["exp_group"] = experiment_group;
        final_stimuli_blocks[i][j]["presented_target"] = final_stimuli_blocks[i][j][experiment_group];
  
    };
  };

//Create final sequence that are presented to the slide
final_item_sequence = [final_stimuli_blocks].flat().flat();
console.log("final sequence", final_item_sequence);


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

    slides.practice_slider = slide({
        name : "practice_slider",
    
        /* trial information for this block
         (the variable 'stim' will change between each of these values,
          and for each of these, present_handle will be run.) */
        present : [{"a": 1}],
        //this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".rating_err").hide();
            $(".rating_errgood").hide();
            $(".question_err").hide();
            $(".question_errgood").hide();
            this.stim = stim;
            $(".prompt").html("<b> The composer thinks that the violinist has three extra tickets to the concert. <\/b>");
            $(".comprehension_question").html("<b>______ might have three extra tickets to the concert.<\/b>");
            $(".comprehension_question_choice_a").html("The composer");
            $(".comprehension_question_choice_b").html("The vioinist");
            $(".comprehension_question_choice_c").html("The box office");
            $(".comprehension_question_choice_d").html("I don't know");
            $(".second_instruction").html("Here's a task about the sentence on the previous page. Please try to answer it to the best of your ability.");
            $(".comprehension").hide();
            $(".second_button").hide();
            $(".second_instruction").hide();

            this.init_sliders();
            exp.sliderPost = null; //erase current slider value
            exp.first_response_wrong = 0;
            exp.first_response_value = null;
            exp.attempts = 0;
        },

        first_button : function() {
            $(".rating_err").hide();
            $(".rating_errgood").hide();
            if (exp.sliderPost == null) {
                $(".rating_err").show();
            } 
            else if (exp.sliderPost < 0.5) {
                exp.first_response_wrong = 1;
                exp.first_response_value =exp.sliderPost;
                exp.attempts = exp.attempts + 1;
                $(".rating_errgood").show();
            }
            else {
                $(".first_button").hide();
                $(".prompt").hide();
                $(".target").hide();
                $(".slider_table").hide();
                $(".first_instruction").hide();
                
                $(".comprehension").show();
                $(".second_button").show();
                $(".second_instruction").show();
                rating = exp.sliderPost;
                exp.sliderPost = null;

                
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                //_stream.apply(this);
            }
        },

        second_button : function(){
            $(".question_err").hide();
            $(".question_errgood").hide();
            exp.sliderPost = $('input[name="choice"]:checked').val();
            if (exp.sliderPost == null) {
                $(".question_err").show();
            } 
            else if (exp.sliderPost != "b" /*&& exp.sliderPost != "d"*/) {
                exp.first_response_wrong = 1;
                exp.first_response_value = exp.sliderPost;
                exp.attempts = exp.attempts + 1;
                $(".question_errgood").show();
            }
            else {
                this.log_responses();
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                _stream.apply(this);
                exp.sliderPost = null;
            }
        },

        init_sliders : function() {
            utils.make_slider("#practice_slider_1", function(event, ui) {
                exp.sliderPost = ui.value;
                
            });
        },       

        log_responses : function() {
            exp.data_trials.push({
                "rating" : rating,
                "answer_of_comprehension" : exp.sliderPost,
                "first_response_value": exp.first_response_value,
                "wrong_attempts": exp.attempts,
                "item_type" : "practice_good",
                "block_sequence": "practice",
                "item_number": "practice_good",
                "trial_sequence_total": 0,
                "group": experiment_group
            });
            console.log(exp.data_trials);
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
        present : [{"a": 1}],
        //this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".rating_err").hide();
            $(".rating_errgood").hide();
            $(".question_err").hide();
            $(".question_errgood").hide();
            this.stim = stim;
            $(".first_instruction").show();
            $(".prompt").html("<b> The to the wonders rehearsal guitarist the whether drummer come will<\/b>");
            $(".comprehension_question").html("<b>______ might come to the rehearsal.<\/b>");
            $(".comprehension_question_choice_a").html("The drummer");
            $(".comprehension_question_choice_b").html("The guitarist");
            $(".comprehension_question_choice_c").html("The director");
            $(".comprehension_question_choice_d").html("I don't know");
            $(".second_instruction").html("Please try to fill in the blank to the best of your ability.");
            $(".comprehension").hide();
            $(".second_button").hide();
            $(".prompt").show();
            $(".slider_table").show();
            $(".first_button").show();
            $(".second_instruction").hide();

            this.init_sliders();
            exp.sliderPost = null; //erase current slider value
            exp.first_response_wrong = 0;
            exp.first_response_value = null;
            exp.attempts = 0;
        },

        first_button : function() {
            $(".rating_err").hide();
            $(".rating_errgood").hide();

            if (exp.sliderPost == null) {
                $(".rating_err").show();
            } 
            else if (exp.sliderPost > 0.5) {
                exp.first_response_wrong = 1;
                exp.first_response_value =exp.sliderPost;
                exp.attempts = exp.attempts + 1;
                $(".rating_errgood").show();
            }
            else {
                $(".first_button").hide();
                $(".prompt").hide();
                $(".target").hide();
                $(".slider_table").hide();
                $(".first_instruction").hide();
                
                $(".comprehension").show();
                $(".second_button").show();
                $(".second_instruction").show();
                rating = exp.sliderPost;
                exp.sliderPost = null;
         
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                //_stream.apply(this);
            }
        },
        
        second_button : function(){
            exp.sliderPost = null;
            $(".question_err").hide();
            $(".question_errgood").hide();
            exp.sliderPost = $('input[name="choice_"]:checked').val();
            console.log(exp.sliderPost);
            if (exp.sliderPost == null) {
                $(".question_err").show();
            } 
            else if (/*exp.sliderPost != "a" && */ exp.sliderPost != "d") {
                exp.first_response_wrong = 1;
                exp.first_response_value = exp.sliderPost;
                exp.attempts = exp.attempts + 1;
                $(".question_errgood").show();
            }
            else {
                this.log_responses();
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                _stream.apply(this);
                exp.sliderPost = null;
            }
        },

        init_sliders : function() {
            utils.make_slider("#practice_slider_2", function(event, ui) {
                exp.sliderPost = ui.value;
                
            });
        },       

        log_responses : function() {
            exp.data_trials.push({
                "rating" : rating,
                "answer_of_comprehension" : exp.sliderPost,
                "first_response_value": exp.first_response_value,
                "wrong_attempts": exp.attempts,
                "item_type" : "practice_good",
                "block_sequence": "practice",
                "item_number": "practice_good",
                "trial_sequence_total": 0,
                "group": experiment_group
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
        present : final_item_sequence,
        
        //this gets run only at the beginning of the block
        present_handle : function(stim) {
            $(".rating_err").hide();
            $(".question_err").hide();
            this.stim = stim; //I like to store this information in the slide so I can record it later.
            $(".target").html(stim.presented_target);
            $(".comprehension_question").html(stim.comprehension_question);
            //Shuffle the answers
            choices = shuffle([0, 1, 2]);
            console.log(choices);
            choices.push(3);
            choices_items = [stim.choice_a, stim.choice_b, stim.choice_c];
            shuffled_items = [];
            for(i = 0; i < 3; i++){
                shuffled_items.push(choices_items[choices[i]]);
            }
            console.log(shuffled_items);
            $(".comprehension_question_choice_a").html(shuffled_items[0]);
            $(".comprehension_question_choice_b").html(shuffled_items[1]);
            $(".comprehension_question_choice_c").html(shuffled_items[2]);
            $(".comprehension_question_choice_d").html(stim.choice_d);
            $(".target").show();
            $(".comprehension").hide();
            $(".slider_table").show();
            $(".first_button").show();
            exp.sliderPost == null
            this.init_sliders()
            exp.sliderPost = null; //erase current slider value
        },
    
        first_button : function() {
            if (exp.sliderPost == null) {
                $(".rating_err").show();
            } else {
                $(".first_button").hide();
                $(".prompt").hide();
                $(".target").hide();
                $(".slider_table").hide();
                $(".rating_err").hide();
                $(".comprehension").show();
                $(".second_button").show();
                rating = exp.sliderPost;
                exp.sliderPost = null;
            }
        },

        second_button : function(){
            exp.sliderPost = null;
            $(".question_err").hide();
            choosed = $('input[name="choice_test"]:checked').val();
            exp.sliderPost = choices[choosed];
            console.log(exp.sliderPost);
            response_correctness = "Incorrect";
            if (exp.sliderPost == 0){
                response_correctness = "Correct";
            }
            else if (exp.sliderPost == 3){
                response_correctness = "Don't know"
            }
            if (exp.sliderPost == null) {
                $(".question_err").show();
            } 
            else {
                this.log_responses();
                /* use _stream.apply(this); if and only if there is
                "present" data. (and only *after* responses are logged) */
                $('input:radio[name="choice_test"]').removeAttr('checked');
                _stream.apply(this);
                exp.sliderPost = null;
            }
            
        },
    
        init_sliders : function() {
            utils.make_slider("#single_slider", function(event, ui) {
                exp.sliderPost = ui.value;
            });
        },
    
        log_responses : function() {
          exp.data_trials.push({
            // item-specific fields
            "rating_response" : rating,
            "comprehension_response" : exp.sliderPost,
            "response_correctness" : response_correctness,
            "choosed_choice" : choosed,
            "item_condition" : this.stim.condition,
            "item_group" : this.stim.exp_group,
            "trial_sequence_total": order,
            "block_sequence": this.stim.new_block_sequence,
            "item_number": this.stim.item,
            // experiment-general fields
            "experiment_condition": this.stim.exp_condition,
          });
          order = order + 1;
        }
      });
    
    console.log(exp.data_trials);

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
                "group" : exp.group,
                "subject_information" : exp.subj_data,
                "time_in_minutes" : (Date.now() - exp.startT)/60000
            };
            proliferate.submit(exp.data);
        }
    });

    return slides;
    

}


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
    exp.structure=["i0", "instructions", "practice_slider", "post_practice_1","practice_slider_bad", "post_practice_2", "last_reminder", "one_slider", "subj_info", "thanks"];
    //"instructions", "practice_slider", "post_practice_1","practice_slider_bad", "post_practice_2", "last_reminder",
    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);
    
    exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

    $('.slide').hide(); //hide everything

    //make sure turkers have accepted HIT (or you're not in mturk)
    $("#start_button").click(function() {
        exp.go();
    });

    exp.go(); //show first slide
}
