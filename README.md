# Meta-Analysis Software

A desktop application for conducting meta-analyses, built with R Shiny and packaged as a standalone Windows app using Electron.

## Author

**Ali Molahassani**
Department of Clinical Nutrition and Biochemistry, Faculty of Medicine, Neyshabur University of Medical Sciences, Neyshabur, Razavi Khorasan, Iran

## Features

- **Pairwise Meta-Analysis** — random-effects models (Hartung-Knapp), forest plots, subgroup analysis
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

- **متا-آنالیز زوجی (Pairwise)** — مدل اثرات تصادفی (Hartung-Knapp)، نمودار Forest، آنالیز زیرگروه
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

