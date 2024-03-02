#!/usr/bin/env python
"""
Prints a random bit of advice from Alex Vermeer's guide to overcoming
procrastination.

Source: https://alexvermeer.com/wp-content/uploads/howtogetmotivated-7100x5000.png

"""
import random

EXPECTANCY: list[str] = [
    "Remember: lack of effort guarantees failure!",
    "Achieve one goal after another",
    "Recognize small improvements as victories",
    "Review your inspirations",
    "Know what inspires you and why",
    "Make your inspirations visible",
    "Ask: what could go wrong?",
    "Compare the ideal state with the current state",
    "Visualize and contrast the present and future",
    "Nothing is carved in stone",
    "Qualities and skills are cultivated through effort",
]

VALUE: list[str] = [
    "Set and review your major life goals",
    "How does this connect to your life goals?",
    "Match difficulty with skill",
    "Too easy? Make it harder. Too hard? Make it easier.",
    "Compete against yourself",
    "Compete against others",
    "Turn it into a game, make it fun!",
    "Get your blood moving",
    "Splash cold water on your face",
    "Energize your environment",
    "Plan around energy, not time",
    "Reward your success",
    "What can you avoid doing by doing this?",
    "Combine long-term interests with short-term goals",
]

IMPULSIVENESS: list[str] = [
    "Set a goal: specific, realistic, meaningful.",
    "Break it down!",
    "Input ('for x minutes') is better than output ('finish this')",
    "'Achieve this' is better than 'avoid that'",
    "Recognize what is tempting you",
    "Eliminate or hide temptations",
    "Recognize what is distracting you",
    "Eliminate or hide distractions",
    "Can part of this be turned into a habit?",
    "Can part of this be added to an existing routine?",
    "Separate work and play",
    "Use goal reminders",
    "Look at your goals",
    "Make your goals visible",
    "Make progress visual",
    "Track your progress",
    "Pair temptations with undesirable images",
]

ALL: list[str] = EXPECTANCY + VALUE + IMPULSIVENESS

if __name__ == "__main__":
    print(random.choice(ALL))
