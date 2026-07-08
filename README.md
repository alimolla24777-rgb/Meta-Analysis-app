# Meta-Analysis Software

A desktop application for conducting meta-analyses, built with R Shiny and packaged as a standalone Windows app using Electron.

## Author

**Ali Molahassani**
Department of Clinical Nutrition and Biochemistry, Faculty of Medicine, Neyshabur University of Medical Sciences, Neyshabur, Razavi Khorasan, Iran

## Features

- **Pairwise Meta-Analysis** — random-effects models (Hartung-Knapp), forest plots, subgroup analysis(continious)
- **Dose-Response Meta-Analysis** — restricted cubic splines, effect prediction for new dose values
- **Meta-Regression** — covariate/moderator analysis with bubble plots
- **Publication Bias Diagnostics** — Egger's test, Begg's test, Trim & Fill, funnel plots
- **Sensitivity Analysis** — Leave-one-out, small-study effects
- **Automated Interpretation** — statistical, methodological, and writing-assistant text generation
- **HTML Report Export** — full analysis report with plots and interpretation

## Tech Stack

- **R / Shiny** — statistical engine and UI (`meta`, `dosresmeta`, `rms`, `bslib`)
- **Electron** — desktop app wrapper with embedded R runtime and pandoc, so no separate R installation is required on the user's machine

## Building from Source

This repository contains the application source code (`app_ui.R`, `app_server.R`, `main.js`). To build the standalone Windows executable yourself:

1. Install [R](https://cran.r-project.org/) and the required packages: `shiny`, `shinyjs`, `meta`, `readxl`, `bslib`, `dplyr`, `rmarkdown`, `dosresmeta`, `rms`
2. Install [Node.js](https://nodejs.org/) and run `npm install`
3. Copy your R installation, package library, and a pandoc build into `R/`, `library/`, and `pandoc/` folders alongside the project
4. Run `npm run dist` to produce a portable `.zip` build

## License

This project is shared for research and educational purposes.

---

# نرم‌افزار متا-آنالیز

یک نرم‌افزار دسکتاپ برای انجام متا-آنالیز، ساخته‌شده با R Shiny و بسته‌بندی‌شده به‌صورت یک اپلیکیشن مستقل ویندوز با استفاده از Electron.

## نویسنده

**علی ملاحسنی**
گروه تغذیه بالینی و بیوشیمی، دانشکده پزشکی، دانشگاه علوم پزشکی نیشابور، نیشابور، خراسان رضوی، ایران

## امکانات

- **متا-آنالیز زوجی (continious)(Pairwise)** — مدل اثرات تصادفی (Hartung-Knapp)، نمودار Forest، آنالیز زیرگروه
- **متا-آنالیز دوز-پاسخ** — اسپلاین‌های مکعبی محدودشده، پیش‌بینی اثر برای دوزهای جدید
- **متا-رگرسیون** — آنالیز کوواریت/مدیریتور با نمودار حبابی
- **تشخیص سوگیری انتشار** — آزمون Egger، آزمون Begg، Trim & Fill، نمودار Funnel
- **آنالیز حساسیت** — Leave-one-out، اثرات مطالعات کوچک
- **تفسیر خودکار** — تولید متن تفسیر آماری، روش‌شناختی، و کمک به نگارش
- **خروجی گزارش HTML** — گزارش کامل آنالیز همراه با نمودارها و تفسیر

## فناوری‌های استفاده‌شده

- **R / Shiny** — موتور آماری و رابط کاربری (`meta`، `dosresmeta`، `rms`، `bslib`)
- **Electron** — پوسته اپلیکیشن دسکتاپ با R و pandoc تعبیه‌شده، بدون نیاز به نصب جداگانه R روی سیستم کاربر

## ساخت از سورس

این مخزن شامل کد سورس اپلیکیشن (`app_ui.R`، `app_server.R`، `main.js`) است. برای ساخت فایل اجرایی مستقل ویندوز:

۱. [R](https://cran.r-project.org/) و پکیج‌های مورد نیاز را نصب کنید: `shiny`، `shinyjs`، `meta`، `readxl`، `bslib`، `dplyr`، `rmarkdown`، `dosresmeta`، `rms`
۲. [Node.js](https://nodejs.org/) را نصب کرده و `npm install` را اجرا کنید
۳. نصب R، کتابخانه پکیج‌ها، و یک build از pandoc را در پوشه‌های `R/`، `library/`، و `pandoc/` کنار پروژه کپی کنید
۴. `npm run dist` را اجرا کنید تا یک نسخه `.zip` قابل‌حمل ساخته شود

## مجوز

این پروژه برای اهداف پژوهشی و آموزشی به اشتراک گذاشته شده است.






<img width="1919" height="988" alt="image" src="https://github.com/user-attachments/assets/3401da7a-9633-443c-afac-96e5fe3c42d6" />
<img width="1919" height="955" alt="image" src="https://github.com/user-attachments/assets/c2d15c7f-060e-4f88-9d57-fd3184555ffa" />
<img width="1915" height="973" alt="Screenshot 2026-07-08 232505" src="https://github.com/user-attachments/assets/8f240c58-1c76-4000-b74e-9bb5a6049664" />
<img width="1919" height="992" alt="image" src="https://github.com/user-attachments/assets/e4789f27-a0aa-422c-8333-fa07e0359b69" />
<img width="1896" height="957" alt="image" src="https://github.com/user-attachments/assets/80a88661-4b21-4e23-adb1-885a749e9507" />
<img width="1891" height="953" alt="image" src="https://github.com/user-attachments/assets/ad40044e-5af5-470a-98e5-f5baed739612" />
<img width="1919" height="907" alt="image" src="https://github.com/user-attachments/assets/f0256a67-e278-4822-b3b0-f370df977d3a" />
<img width="1899" height="955" alt="image" src="https://github.com/user-attachments/assets/57be8e2f-0fd9-4789-97e8-e10ffbc89463" />
<img width="1919" height="981" alt="image" src="https://github.com/user-attachments/assets/35895f79-66f7-4d25-b5ed-9f7805407fae" />
<img width="1918" height="962" alt="image" src="https://github.com/user-attachments/assets/f9ea2991-80b6-49ef-88d5-8a77d4a2de75" />


