{-# LANGUAGE OverloadedStrings #-}

module Templates (portfolioPage) where

import Lucid
import Data.Text (Text)

data Project = Project { title :: Text, link :: Text, description :: Text, duration :: Text }
data Certification = Certification { certName :: Text, certLink :: Maybe Text }

projects :: [Project]
projects =
  [ Project "Checkmate Arena" "https://checkmatearena-nikm.onrender.com" 
    "Online chess platform with live matches, puzzles, Sudoku, and Solitaire." "May 2025 - Present"
  , Project "Code Comparator" "https://codecomparator.vercel.app"
    "AI-powered tool for comparing code snippets and visual diffs." "Apr 2025 - Present"
  , Project "Daily Scope News" "https://daily-scope-news.vercel.app/"
    "Frontend news site with real-time updates from multiple sources." "May 2025 - Present"
  , Project "SceneRadar" "https://sceneradar.onrender.com"
    "Web platform for trending movies and shows." "May 2025 - Present"
  , Project "ResumeCraft" "https://craftresume.vercel.app"
    "AI-powered resume builder with ATS optimization." "Apr 2025 - Present"
  , Project "Updevted" "https://updevted.onrender.com"
    "Platform for developer tools and trends." "Apr 2025 - Present"
  , Project "LegonAttend" "https://legonattend.vercel.app"
    "QR-based attendance system for lecturers." "In Development"
  ]

certifications :: [Certification]
certifications =
  [ Certification "Electronic Arts - Software Engineering Job Simulation" 
    (Just "https://forage-uploads-prod.s3.amazonaws.com/completion-certificates/j43dGscQHtJJ57N54/a77WE3de8qrxWferQ_j43dGscQHtJJ57N54_pYuSGdn3uHut7KKXd_1744393691028_completion_certificate.pdf")
  , Certification "Introduction to IoT and Digital Transformation - Cisco"
    (Just "https://www.credly.com/badges/5c8a06d8-112b-44fe-b4c6-c0d69a4c1248/linked_in_profile")
  , Certification "Java Fundamentals - HackerRank"
    (Just "https://www.hackerrank.com/certificates/iframe/d376f9af090d")
  , Certification "Operating Systems Fundamentals - Cisco"
    (Just "https://www.credly.com/badges/86523085-453d-4fce-bb80-a7ec7f258c21/linked_in_profile")
  , Certification "Principles of Agentic Code Development" Nothing
  , Certification "Python for Data Science and AI Development - IBM" Nothing
  ]

portfolioPage :: Html ()
portfolioPage = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            title_ "Kimathi Sedegah – Tech Developer"
            link_ [rel_ "icon", href_ "/static/images/CodeCad.png"]
            link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&display=swap"]
            link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"]
            link_ [rel_ "stylesheet", href_ "/static/css/style.css"]
        body_ [class_ "dark-mode"] $ do
            nav_ $ do
                div_ [class_ "logo"] "kimathi sedegah."
                div_ [class_ "nav-links"] $ do
                    a_ [href_ "#about"] "About"
                    a_ [href_ "#experience"] "Experience"
                    a_ [href_ "#education"] "Education"
                    a_ [href_ "#certifications"] "Certifications"
                    a_ [href_ "#projects"] "Projects"
                    a_ [href_ "#contact", class_ "cta-button"] "Let's talk"
            button_ [id_ "toggle-dark-mode", class_ "moon-button", ariaLabel_ "Switch to Light Mode"] "☀️"

            main_ [class_ "hero"] $ do
                div_ [class_ "hero-content"] $ do
                    h1_ "Tech Developer | Creating Innovative Solutions for Real-World Challenges"
                    div_ [id_ "typewriter-wrapper"] $ h1_ [id_ "typewriter"] ""
                    p_ [class_ "bio"] "I am a passionate and driven tech enthusiast specializing in Python, JavaScript, PHP, SQL, and cloud technologies like Docker and Anaconda. Currently pursuing a degree at the University of Ghana with a focus on AI & Cloud computing."
                div_ [class_ "hero-image"] $ do
                    img_ [src_ "/static/images/profile.jpg", alt_ "Profile"]
                    div_ [class_ "floating-badge"] "tech development & AI"

            section_ [id_ "projects", class_ "section"] $ do
                h2_ "Projects"
                div_ [class_ "project-grid"] $
                    mapM_ renderProject projects

            section_ [id_ "certifications", class_ "section"] $ do
                h2_ "Certifications"
                div_ [class_ "cert-grid"] $
                    mapM_ renderCert certifications

            section_ [id_ "contact", class_ "contact-section"] $ do
                h2_ "Let's Talk"
                div_ [class_ "contact-info"] $ do
                    a_ [href_ "mailto:sedegahkimathi@gmail.com", class_ "contact-email"] $ do
                        i_ [class_ "fas fa-envelope"] mempty
                        " sedegahkimathi@gmail.com"
                    a_ [href_ "tel:+233209253226", class_ "contact-phone"] $ do
                        i_ [class_ "fas fa-phone"] mempty
                        " +233209253226"
                    a_ [href_ "https://www.linkedin.com/in/kimathi-sedegah-227a40347", target_ "_blank", class_ "contact-linkedin"] $ do
                        i_ [class_ "fab fa-linkedin"] mempty
                        " www.linkedin.com/in/kimathi-sedegah-227a40347"

            script_ [src_ "/static/js/script.js"] ("" :: Text)

renderProject :: Project -> Html ()
renderProject p = 
    div_ [class_ "project-item"] $ do
        h3_ (toHtml $ title p)
        p_ [class_ "duration"] (toHtml $ duration p)
        p_ (toHtml $ description p)
        p_ $ a_ [href_ (link p), class_ "project-link", target_ "_blank", rel_ "noopener noreferrer"] "View Project"

renderCert :: Certification -> Html ()
renderCert c = 
    case certLink c of
        Just linkUrl -> 
            a_ [href_ linkUrl, target_ "_blank", class_ "cert-item", rel_ "noopener noreferrer"] $ do
                toHtml (certName c)
                i_ [class_ "fas fa-external-link-alt link-icon"] mempty
        Nothing -> div_ [class_ "cert-item"] (toHtml $ certName c)
