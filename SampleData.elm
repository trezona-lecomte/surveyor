module SampleData exposing (questions)

import Types exposing (..)


questions : List Question
questions =
    [ MultiChoice { editing = False, prompt = "Please tick status:", options = [ "Job-seeker", "Employer" ], answer = "" }
      -- , NumberRange { editing = False, prompt = "Age", range = ( 0, 120 ), answer = 0 }
    , MultiChoice { editing = False, prompt = "Do you have a LinkedIn or other professional networking account?", options = [ "Yes", "No" ], answer = "" }
    , MultiChoice { editing = False, prompt = "How often do you use your LinkedIn / other professional networking account?", options = [ "Every day", "Every week", "Every month", "Seldom" ], answer = "" }
    , MultiChoice { editing = False, prompt = "How many jobs opportunities have you found via your LinkedIn / other professional networking account?", options = [ "None", "One", "More than one" ], answer = "" }
    , MultiChoice { editing = False, prompt = "Do you / have you ever had a business mentor?", options = [ "Yes", "No" ], answer = "" }
    , MultiChoice { editing = False, prompt = "Would you use an online service that allows employers to rate your professional performance and makes this information available to other employers?", options = [ "Yes", "No" ], answer = "" }
    , MultiChoice { editing = False, prompt = "Would you use an online service that allowed you to find and contact professional mentors?", options = [ "Yes", "No" ], answer = "" }
    , OpenEnded { editing = False, prompt = "How much would you pay per year for an online service that provides you with access to a mentors from a range of industries?", answer = "" }
    , OpenEnded { editing = False, prompt = "How much would you pay per year for an online service that allows you to directly offer your services to employers on a short-term / trial basis?", answer = "" }
    , OpenEnded { editing = False, prompt = "How much would you pay for an online service that allows you to easily establish your own cooperative job-seeking / head-hunting company, and then recruit friends and colleagues to join?", answer = "" }
    ]



-- EMPLOYER / OVER AGE 40
-- "Do you have a LinkedIn or other professional networking account?  If other, please specify?"
-- "How often do you use your LinkedIn / other professional networking account?"
-- "How many jobs opportunities have you found via your LinkedIn / other professional networking account?"
-- "Have you every hired someone via LinkedIn / other professional networking account?  If yes, how many?"
-- "Are you / have you ever been a business mentor?"
-- "Do you / have you ever had a business mentor?"
-- "How much would you pay for an online service that allowed you to find and contact professional mentors?"
-- "How much would you pay for an online service that allowed you to offer your expertise as a professional mentor to individuals / groups, with the option of negotiating a fee for your services?"
-- "How much would you pay per year for an online service that allowed you to search through a repository of CVs, aggregated by industry and other relevant search categories, and engage selected candidates?"
-- "How much would you pay per year for an online service that allowed you to offer candidates short-term / trial employment?"
