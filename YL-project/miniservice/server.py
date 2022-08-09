from flask import Flask, render_template, request, redirect
from sklearn import tree
from joblib import load

clf = load("model.joblib")

app = Flask(__name__)

questions = {
    "V201115": {
        "type": "radio",
        "text": "How hopeful do you feel about how things are going in the country these days?",
        "options": {
            "1": "Not at all",
            "2": "A little",
            "3": "Somewhat",
            "4": "Very",
            "5": "Extremely"
        }
    },
    "V201121": {
        "type": "radio",
        "text": "How proud do you feel about how things are going in the country these days?",
        "options": {
            "1": "Not at all",
            "2": "A little",
            "3": "Somewhat",
            "4": "Very",
            "5": "Extremely"
        }
    },
    "V201124": {
        "type": "radio",
        "text": "Do you approve or disapprove of the way the U.S. Congress has been handling its job?",
        "options": {
            "1": "Approve",
            "2": "Disapprove"
        }
    },
    "V201130": {
        "type": "radio",
        "text": "Do you approve or disapprove of the way Donald Trump is handling the economy?",
        "options": {
            "1": "Approve",
            "2": "Disapprove"
        }
    },
    "V201142": {
        "type": "radio",
        "text": "Do you approve or disapprove of the way Donald Trump has handled the coronavirus, or COVID-19, pandemic?",
        "options": {
            "1": "Approve",
            "2": "Disapprove"
        }
    },
    "V201136": {
        "type": "radio",
        "text": "Do you approve or disapprove of the way Donald Trump has handling health care?",
        "options": {
            "1": "Approve",
            "2": "Disapprove"
        }
    },
    "V201139": {
        "type": "radio",
        "text": "Do you approve or disapprove of the way Donald Trump has handling immigration?",
        "options": {
            "1": "Approve",
            "2": "Disapprove"
        }
    },
    "thermometer-notice": {
        "type": "desc",
        "text": "On the next few questions, you will be asked to use a rating thermometer. Ratings between 50 and 100 mean you feel favorable toward the person. Ratings between 0 and 50 mean that you don't feel favorable toward the person. You would rate the person at the 50 degree mark if you don't feel particularly warm or cold toward the person. "
    },
    "V202143": {
        "type": "text",
        "text": "On the feeling thermometer scale from 0 to 100, how would you rate Joe Biden?"
    },
    "V202144": {
        "type": "text",
        "text": "On the feeling thermometer scale from 0 to 100, how would you rate Donald Trump?"
    },
    "V201324": {
        "type": "radio",
        "text": "What do you think about the state of the economy these days in the United States?",
        "options": {
            "1": "Very good",
            "2": "Good",
            "3": "Neither good nor bad",
            "4": "Bad",
            "5": "Very bad"
        }
    },
    "V201336": {
        "type": "radio",
        "text": "Which of the following options BEST represents your opinion on abortion?",
        "options": {
            "1": "By law, abortion should never be permitted",
            "2": "The law should permit abortion only in case of rape, incest, or when the woman's life is in danger",
            "3": "The law should permit abortion other than for rape/incest/danger to woman but only after need clearly established",
            "4": "By law, a woman should always be able to obtain an abortion as a matter of personal choice. "
        }
    }
}


@app.route("/")
def index():
    return redirect("/index.html")


@app.route("/index.html")
def hello():
    return render_template('index.html', questions=questions)


@app.route("/submitted.html", methods=["POST"])
def submit():
    return render_template('submitted.html', result=clf.predict([[
        request.form["V201115"],
        request.form["V201121"],
        request.form["V201124"],
        request.form["V201130"],
        request.form["V201142"],
        request.form["V201136"],
        request.form["V201139"],
        request.form["V202143"],
        request.form["V202144"],
        request.form["V201324"],
        request.form["V201336"],
    ]])[0], candidates={
        -1: ["Inapplicable - you do not intend to vote for a president",
             "https://upload.wikimedia.org/wikipedia/commons/b/b4/Circle_question_mark.png"],
        1: ["Joe Biden (DEMOCRAT)",
            "https://upload.wikimedia.org/wikipedia/commons/6/68/Joe_Biden_presidential_portrait.jpg"],
        2: ["Donald Trump (REPUBLICAN)",
            "https://upload.wikimedia.org/wikipedia/commons/5/56/Donald_Trump_official_portrait.jpg"],
        3: ["Jo Jorgensen (INDEPENDENT)",
            "https://upload.wikimedia.org/wikipedia/commons/3/3f/Jo_Jorgensen_portrait_3.jpg"],
        4: ["Howie Hawkins",
            "https://upload.wikimedia.org/wikipedia/commons/d/df/Hawkins_2010.jpg"],
        5: ["Other Candidate",
            "https://upload.wikimedia.org/wikipedia/commons/b/b4/Circle_question_mark.png"]
    })
