

# Session materials

## Sessions

The study consists of six sessions. 

- **Session 1. Measurement of individual differences in executive functions and language history.**
  This session is performed by participants online and on their own. The session for the Norwegian
  site is available [here](https://app.gorilla.sc/openmaterials/782880). The session for the Spanish 
  site is available [here](https://app.gorilla.sc/openmaterials/782877). In both sites, the session 
  comprises the following parts:

  - **1.1. Basic participant data** collected in Gorilla
  
  - **1.2. Consent form** presented in Gorilla

  - **1.3. Cognitive battery administered in Gorilla.** This battery measures attention at large, 
   which is relevant because previous studies have suggested that attention may be a precursor to
   the selection of a source of transfer in L3 learning
   ([González Alonso et al., 2020](https://doi.org/10.1016/j.jneuroling.2020.100939);
   [Pereira Soares et al., 2022](https://doi.org/10.3390/brainsci12050669)). The cognitive battery
   for the Norwegian site is available [here](https://app.gorilla.sc/openmaterials/782880), whereas 
   the battery for the Spanish site is available [here](https://app.gorilla.sc/openmaterials/782877). 

     The battery comprises the following tasks, administered in a counterbalanced order 
     across participants.
  
    - **Stroop task:** primarily measures inhibitory control, and contains 50 congruent and 
      50 incongruent trials;
          
    - **forward and backward digit span tasks:** primarily measure working memory, and each 
      contains six levels of difficulty (i.e., 3—8 digits to recall), with four trials 
      per level, adding up to a total of 24 trials in the forward task, and another 24 
      trials in the backward task;
          
    - **alternating serial reaction time task:** primarily measures implicit learning, and 
      contains 25 blocks with 85 trials per block.

  - **1.4. Language History Questionnaire (LHQ3)**, administered on the [LHQ3 website](https://lhq-blclab.org).

#### ⏱ *One-week delay*

  - **Session 2. Resting-state EEG, followed by work on the property of gender agreement.** This is a 
    lab-based session that closely resembles the procedure of
    [González Alonso et al. (2020)](https://doi.org/10.1016/j.jneuroling.2020.100939)). Session 2 is
    similar to the session implemented by González Alonso et al. (2020), bar two main differences.
    Session 2 begins with a measurement of the resting-state EEG, and it does not include a gender
    assignment task at the end. The session comprises the following parts:
	
    - 2.1. Resting-state EEG
    - 2.2. Pre-training
    - 2.3. Training
    	- 2.3a. Test
    - 2.4. Experiment

#### ⏱ *One-week delay*

  - **Session 3. Differential object marking and the previous grammatical property.** 
    This is a lab-based session comprising the following parts:

    - 3.1. Pre-training
    - 3.2. Training
    	- 3.2a. Test
    - 3.3. Experiment

#### ⏱ *One-week delay*

  - **Session 4. Verb-object number agreement and the previous grammatical properties.** 
    This is a lab-based session comprising the following parts:

    - 4.1. Training
    	- 4.1a. Test
    - 4.2. Experiment

#### ⏱ *One-week delay*

  - **Session 5. Retest of the cognitive battery that was administered in Session 1**, 
    similarly performed by participants online and on their own.

#### ⏱ *Four-month delay*

  - **Session 6. Retest of all properties, followed by control tests in the relevant 
    natural languages.** The session comprises the following parts:

    - 6.1. Experiment
    
    - 6.2. Control tests in the natural languages, namely,
    
      - in Norwegian site: gender assignment task in Norwegian;

      - in Spanish site: gender assignment task and differential object marking task in Spanish

       These control tests follow the example of the gender assignment task that was administered 
       by González Alonso et al. (2020). It should be noted that no control test on verb-object 
       agreement is administered, as this grammatical property does not exist in English, 
       Norwegian or Spanish.


## Further information about lab-based sessions 2, 3, 4 and 6

### Post-training test

In Sessions 2, 3 and 4, a test is administered after the training. At the end of the test, the score 
is shown on the screen. If the score exceeds 80%, the experimenter must press the letter `C` twice 
to let the experiment part begin. If the score does not exceed 80%, the training and the test are 
automatically repeated. At the end of the second attempt, the score is shown again, and the 
experimenter must press the letter `C` twice to let the experiment begin (cf. González Alonso et 
al., 2020). In the results files, the variable `test_passed` will only appear as `yes` if the score 
in either test attempt exceeded 80%.

### Stimuli

The stimulus lists are described in the R functions that were used to create the stimuli, as well as
in the `list` column in the stimulus files.

### Software

[OpenSesame Version 3.3.14](https://osdoc.cogsci.nl/3.3) was used to present the stimuli and to 
collect behavioural responses. This version can be downloaded 
[here](https://github.com/open-cogsci/OpenSesame/releases/download/release%2F3.3.14/opensesame_3.3.14-py37-win64-1.exe).
The OpenSesame materials of the current study may not be readily compatible with later versions of 
the software, although they could be adapted if necessary.

The experimenters are advised to avoid inadvertently moving or modifying any of the OpenSesame items 
in the left sidebar of the OpenSesame interface. 


### Participant-specific parameters

Each participant was assigned certain parameters in advance, including the mini-language, the order 
of the resting-state parts, and the stimulus lists. For instance, in the Norwegian site, odd-numbered 
participants (i.e., 1, 3, 5...) belong to the Mini-English group, whereas even-numbered participants 
belong to the Mini-Norwegian group. 

* [Participant-specific parameters for Norwegian site](https://github.com/pablobernabeu/materials_L3_transfer_longitudinal/blob/main/session_materials/parameters%20per%20participant/Norway%20site%2C%20parameters%20per%20participant.csv)

* [Participant-specific parameters for Spanish site](https://github.com/pablobernabeu/materials_L3_transfer_longitudinal/blob/main/session_materials/parameters%20per%20participant/Spain%20site%2C%20parameters%20per%20participant.csv)

Due to this pre-assignment, the number of participant IDs that can be used in OpenSesame is 
determined by the number of participants available in each of the files above. 

The code used to assign these parameters is available in the `stimulus_preparation` folder inside 
[materials_L3_transfer_longitudinal.zip](https://osf.io/wbjyr). The file can also be consulted more 
directly [on GitHub](https://github.com/pablobernabeu/materials_L3_transfer_longitudinal/blob/main/stimulus_preparation/participant_parameters.R). 

### Versions

The behavioural results files (logfiles) contain a column named `materials_version`. For details on 
these versions, see README file in the `stimulus_preparation` folder.


### General procedure

At the beginning of the lab-based sessions, the experimenter will first signal the lab is busy using 
a light or a sign. Next, they will ascertain what participant and what session applies. This is done 
using the session logbook that is shared among all session conductors. This session logbook is 
instantly updated online using a cloud service, such as OneDrive. 

Next, the experimenter starts OpenSesame by opening the program directly (not by opening the 
session-specific file), and then opens the appropriate session within OpenSesame. This procedure 
helps prevent the opening of a standalone Python window, the closing of which would result in the 
closing of OpenSesame. Next, the experimenter opens BrainVision Recorder.

When the participant arrives in the lab, they are informed that they can use the toilet outside. 
The participant is also offered some water. 

Next, the size of their head is measured, and an appropriate cap is tried on the participant’s 
head. Next, the cap is placed on a dummy head, and the electrodes are attached to the cap. At 
that point, to prevent signal interference, the participant is kindly asked to either put their 
mobile devices (phone, tablet, smartwatch) in flight mode, or to leave them outside of the booth 
in silent mode. 

Next, to protect the participant's clothes from any drops of gel, a towel is placed on their 
upper back, covering shoulders and upper torso. Both ends of the towel are clipped together at 
the front using two or three clothes pegs. Next, the cap is fitted on the participant's head. 
To prevent the cap from being pulled back during the session, the splitter box is attached to 
the towel on the participant's back, right below their head. Next, measures are taken to adjust 
the position of the cap evenly, first from the nasion to the inion, and then from the tip of an 
ear to the other ear. 

Next, the experimenter returns to OpenSesame and runs the session in full screen by clicking on 
the full green triangle at the top left. Then, a file explorer window opens, in which the 
experimenter must assign a subject number consistent with the session logbook, and must select 
the destination folder for the logfile. The destination folder is called `logfiles`. Any prompts 
to overwrite a logfile must normally be refused, or considered carefully, due to the risk of 
losing data from previous sessions.

In the first screen, the experimenter can disable some of the tasks. This option can be used if a 
session has ended abruptly, in which case the session can be resumed from a near checkpoint. In 
such a case, the experimenter must first note this incident in their logbook, and rename the log 
file that was produced on the first run, by appending `_first_run` to the name. This prevents 
overwriting the file on the second run. Next, they must open a new session, enter the same 
participant ID, and select the appropriate part from which to begin. This part must be the part 
immediately following the last part that was completed in full. For instance, if a session ended
abruptly during the experiment, the beginning selected on the second run would be the experiment. 
Once the session has finished completely, the first log file and the second log file must be 
safely merged into a single file, keeping only the fully completed tasks.

In the first instructional screen, participants are asked to refrain from asking any questions 
unless it is necessary, so that all participants can receive the same instructions.

At the beginning of the resting-state part in Session 2, and at the beginning of the Experiment 
part, instructions are presented on the screen that ask participants to stay as still as possible 
during the following task. The screen contains an orange-coloured square with the letters `i.s.r`, 
that remind the experimenter to check the impedance and the signal, and finally to begin recording 
the EEG signal. If the impedance of any electrodes is poor, the experimenter may enter the booth 
to lower the impedance of the electrodes affected. Otherwise, after validating the signal and the 
impedance, the experimenter can begin the recording in BrainVision, and press the letter `C` twice 
in the stimulus computer. At that point, a green circle will appear, along with instructions for 
the participant. 

Similarly, at the end of the eyes-closed resting-state measurement (which is five minutes long), 
the experimenter must intervene when they see the screen with the orange stripes, by knocking on 
the door to let the participant open their eyes. 

Furthermore, at the end of the resting-state part and at the end of the Experiment part, a screen
with a crossed-out R appears to remind the experimenter to stop recording the EEG. 

Notice that the above-mentioned stages, characterised by screens with orange stripes, require the 
experimenter's intervention. The experimenter must allow the participant to read any text on these 
screens. Next, the experimenter must press the letter `C` twice to let the session continue. This 
protocol provides the experimenter with control when necessary. The experimenter should be aware 
of the use of the letter `C` at these points, as the requirement is not signalled on the screen 
to prevent participants from pressing the letter themselves. 

During the experiment, it is important to monitor the EEG signal. If it ever becomes very noisy, 
the experimenter must wait until the next break and the participant to stop, so that the signal 
can be verified. If the noise in the signal is due to the participant's movement, they should be 
asked again to please stay as still possible. If the noise is due to an increase in the 
impedance of some electrodes, the impedance of those electrodes should be revised.

The experiment in Session 2 contains breaks every 40 trials, whereas the experiments in subsequent 
sessions contain breaks every 50 trials. During these breaks, the number of the current trial 
appears in grey on the bottom right corner of the screen.

If a session ends abruptly during the experiment, but there is not enough time to restart the 
session from the experiment, then the data should be uploaded to the repository.

At the end of the session, the data are exported and uploaded to the online repository.


### Definition of items in OpenSesame (only for development purposes, not for in-session use)

  - Each major part of the session is contained in a sequence item that is named in capital 
     letters (e.g., `PRETRAINING`, `TRAINING`, `TEST`, `EXPERIMENT`).

  - `continue_c`: allows proceeding to the following screen after pressing the letter `C`, 
     which should be done by the experimenter. 

### Variables in the OpenSesame log files

In the log files produced by OpenSesame, each part of the session (e.g., Test, Experiment) is 
identified in the variable `session_part`. The names of the response variables are `response`,
`response_time` and `correct`. Item-specific response variables follow the formats of 
`response_[item_name]`, `response_time_[item_name]` and `correct_[item_name]` 
(see [here](https://osdoc.cogsci.nl/3.3/manual/variables/#response-variables)).

The output is verbose and requires preprocessing of the data. For instance, the last response 
in each loop may appear twice in the output, due to the processing of the response. These 
duplicates can—and must—be cleaned up by discarding the rows that have the same trial number
as the preceding row.


### EEG triggers

Triggers are sent from OpenSesame to the EEG recorder throughout the experiment. These triggers 
mark events such as the onset of stimuli, which is essential for analysis of event-related 
potentials. The system for sending triggers is set up in OpenSesame in the inline script 
`EEG_trigger_setup`. It is based on Python code.

The complete key to the triggers is provided below.

  - **0:** reset trigger port. This zero trigger is integrated in the trigger-sending function, 
     and is sent after each of the triggers specified below.

#### Resting-state EEG part

  - **10:** beginning of eyes-open resting-state EEG
  
  - **11:** end of eyes-open resting-state EEG

  - **12:** beginning of eyes-closed resting-state EEG

  - **13:** end of eyes-closed resting-state EEG

#### Experiment part

  - **254:** beginning of Experiment part

  - **5:** fixation mark in each trial

#### ID of each target word (only applicable to target trials)

  - **40–99:** triggers starting from 40 and lower than 99. Time-locked 
to the onset of the word of interest in each trial.

#### ID of each target sentence (only applicable to target trials)

  - **110–253:** triggers ranging between 110 and 253

#### Grammatical property of interest (only applicable to target trials)

  - **1:** gender agreement

  - **2:** differential object marking

  - **3:** verb-object number agreement

#### Grammaticality condition (only applicable to target trials)

  - **101:** correct 

  - **102:** violation of interest, namely,
    
      - gender agreement violation (when the property of interest is gender agreement, 
        marked by trigger 1); or
      - violation of differential object marking (when the property of interest is
        differential object marking, marked by trigger 2); or
      - violation of verb-object number agreement (when the property of interest is 
        verb-object number agreement, marked by trigger 3)

  - **103:** ancillary violation, namely,

      - number agreement violation (when the property of interest is gender agreement); or
      - article location violation (when the property of interest is differential 
        object marking or verb-object number agreement)

#### Accuracy

  - **6:** correct response 
  
  - **7:** incorrect response 

  - **8:** end of trial (marker only present in Session 2)

  - **255:** end of Experiment part

### Timing of the triggers for target words

A further note is needed regarding the triggers that are sent with the word of interest in
each trial. The trigger that is sent at the exact onset of the word is the ID of the word. 
Ten milliseconds later, another trigger marks the ID of the sentence. After another ten 
milliseconds, a trigger marks the current grammatical property of interest. Yet another 
ten milliseconds later, a trigger marks the grammaticality condition. 

