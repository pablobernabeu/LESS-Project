

The preparation of the stimuli draws on a modular framework formed of interoperable components. First, the minimal components of each language are contained in a base file in the ‘stimulus_preparation’ folder. Second, the linguistic and visual stimuli finally presented are created by assembling minimal components. Several controls are exerted on the stimuli to prevent spurious effects. For instance, gender and number are counterbalanced across experimental conditions. Similarly, words and experimental conditions within the same set appear equally often. All the stimuli are compiled in the script 'compile_all_stimuli.R', and the resulting stimuli are saved to the 'session_materials' folder. In conclusion, the present framework facilitates the reproducibility and the inspection of the stimuli, and allows a scalable extension to accommodate different features and languages.

Parallel lists of stimuli are used to enable some of the controls. The lists are numbered in the names of the files and they are described in the 'list' column in each file. 

The open-source software OpenSesame is used to present the stimuli and collect participants' responses.


== Materials versions ==

The materials are version-controlled, and the release notes follow below.

* v1.0.0

  - First release


* v1.0.1

  - Revise the location of articles in 'article location violation' condition.


* v1.0.2 

  - Revise the number of items in the images depicting locative sentences in the training phases.
  
  - Revise operation that was not applied at the end of some files.
  
  - Revise match between the IDs of Norwegian nouns and the IDs of their English counterparts.


* v1.0.3 

  - Remove unnecessary pause at the end of the experiment in Session 2.
  
  - Fix error in the number of items shown in the vocabulary pretraining of Session 3.

  - Finish Session 4.

  - Add information to README file in 'session_materials' folder regarding the cognitive battery
    of Session 1 and the procedure of the lab-based sessions.


* v1.0.4

  - Correct minor orthographic typos in the accuracy information that is provided at the end of 
    the tests.

  - Add Session 6. The Spanish site version of this session currently lacks a control test at the 
    end of the session, which could not be finished by the time of the preregistration. In this 
    test, participants will provide grammaticality judgements on the use of differential object 
    marking in Spanish, as a control test to ascertain that they command the property of 
    differential object marking in Spanish. Thus, the purpose of this test is akin to the purpose 
    of the gender assignment task that was administered by González Alonso et al. (2020), which 
    is also part of our current materials. As soon as we have finished the control test on 
    differential object marking, it will be added to the Session 6 of the Spanish site, in time 
    so that all instances of the session include the test. We will then update the materials in 
    the zip folder ‘L3_transfer_longitudinal.zip’ at 
    https://osf.io/974k8/?view_only=7e8aaea4488b442a8ac126a702499c5f.


* v1.1.0

  - Correct major error in the experiment part of Session 4. In previous versions, the trigger 3, 
    which marks the property of verb-object agreement, was incorrectly replaced by 2. As a result, 
    the trials on verb-object agreement could not be distinguished from the trials on differential 
    object marking (correctly marked by trigger 2).


* v1.1.1

  - Correct minor error in the test part of all sessions. In previous versions, if a participant 
    attempted the test for a second time, and did not score above 80%, the session would continue 
    to the experiment. This was a bug, which has been solved in the current version. 


* v1.2.0

  - Fix bug in Sessions 3 and 4 of the Spanish site, whereby the ERP triggers corresponding to 
    the third word in the experiment were not sent. Consistently with all other sessions, these 
    triggers are now sent as and when appropriate.


* v1.2.1

  - Modify protocol after second attempt at the test, motivated by some modest accuracy rates that
    were observed in some of the initial sessions. With the current revision, the experiment part 
    is administered in all cases, regardless of the result of the second attempt. This contrasts 
    with the procedure of González Alonso et al. (2020), where participants could only complete 
    the experiment following a score above 80% in the test. In the current version, to keep some 
    consistency with our previous versions and with González Alonso et al. (2020), the variable 
    'test_passed' only takes the value 'yes' if a score above 80% is achieved.


* v1.2.2

  - Fix bug in Session 6 that caused crash in the gender assignment task. To this end, the 
    'session' variable was removed from the stimulus files of the experiment part, to 
    prevent it from interfering with the variable that is set in OpenSesame.
    
  - Ensure the recording of the session end time in the logfiles produced by OpenSesame. 
    To this end, a logger item was added after the inline script 'session_end_time'.


