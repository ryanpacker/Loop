//
//  DoseMath.swift
//  Naterade
//
//  Created by Nathan Racklyeft on 3/8/16.
//  Copyright © 2016 Nathan Racklyeft. All rights reserved.
//

import Foundation
import HealthKit
import LoopKit

// An InsulinCorrection can be one of four types, and each type can have different associated values.
// When an insulin correction is created, one of these four types must be set with associated values
// I'm still trying to figure out the meaning of each of those values. Current best guess:
//   - min: the lowest GlucoseValue from the collection of predicted GlucoseValues
//   - correcting: Not sure, but I think the eventual BG - last of the predicted values?
//   - minTarget: The bottom of the correction range?
//   - units: The units of insulin associated with the correction

private enum InsulinCorrection {
    case inRange
    case aboveRange(min: GlucoseValue, correcting: GlucoseValue, minTarget: HKQuantity, units: Double)
    case entirelyBelowRange(correcting: GlucoseValue, minTarget: HKQuantity, units: Double)
    case suspend(min: GlucoseValue)
}


extension InsulinCorrection {
    /// The delivery units for the correction
    /// This simply provides access to the Double units that was provided when the InsulinCorrection was created (or zero for types that don't have units)
    private var units: Double {
        switch self {
        case .aboveRange(min: _, correcting: _, minTarget: _, units: let units):
            return units
        case .entirelyBelowRange(correcting: _, minTarget: _, units: let units):
            return units
        case .inRange, .suspend:
            return 0
        }
    }

    /// Determines the temp basal over `duration` needed to perform the correction.
    ///
    /// - Parameters:
    ///   - scheduledBasalRate: The scheduled basal rate at the time the correction is delivered
    ///   - maxBasalRate: The maximum allowed basal rate
    ///   - duration: The duration of the temporary basal
    ///   - rateRounder: The smallest fraction of a unit supported in basal delivery
    /// - Returns: A temp basal recommendation
    fileprivate func asTempBasal(
        scheduledBasalRate: Double,
        maxBasalRate: Double,
        duration: TimeInterval,
        rateRounder: ((Double) -> Double)?
    ) -> TempBasalRecommendation {
        var rate = units / (duration / TimeInterval(hours: 1))  // units/hour
        switch self {
        case .aboveRange, .inRange, .entirelyBelowRange:
            rate += scheduledBasalRate
        case .suspend:
            break
        }

        rate = Swift.min(maxBasalRate, Swift.max(0, rate))

        rate = rateRounder?(rate) ?? rate

        return TempBasalRecommendation(
            unitsPerHour: rate,
            duration: duration
        )
    }

    private var bolusRecommendationNotice: BolusRecommendationNotice? {
        switch self {
        case .suspend(min: let minimum):
            return .glucoseBelowSuspendThreshold(minGlucose: minimum)
        case .inRange, .entirelyBelowRange:
            return nil
        case .aboveRange(min: let min, correcting: _, minTarget: let target, units: let units):
            if units > 0 && min.quantity < target {
                return .predictedGlucoseBelowTarget(minGlucose: min)
            } else {
                return nil
            }
        }
    }

    /// Determins the bolus needed to perform the correction
    ///
    /// - Parameters:
    ///   - pendingInsulin: The number of units expected to be delivered, but not yet reflected in the correction
    ///   - maxBolus: The maximum allowable bolus value in units
    ///   - volumeRounder: The smallest fraction of a unit supported in bolus delivery
    /// - Returns: A bolus recommendation
    fileprivate func asBolus(
        pendingInsulin: Double,
        maxBolus: Double,
        volumeRounder: ((Double) -> Double)?
    ) -> BolusRecommendation {
        var units = self.units - pendingInsulin
        units = Swift.min(maxBolus, Swift.max(0, units))
        units = volumeRounder?(units) ?? units

        return BolusRecommendation(
            amount: units,
            pendingInsulin: pendingInsulin,
            notice: bolusRecommendationNotice
        )
    }
}


struct TempBasalRecommendation: Equatable {
    let unitsPerHour: Double
    let duration: TimeInterval

    /// A special command which cancels any existing temp basals
    static var cancel: TempBasalRecommendation {
        return self.init(unitsPerHour: 0, duration: 0)
    }
}


extension TempBasalRecommendation {
    /// Equates the recommended rate with another rate
    ///
    /// - Parameter unitsPerHour: The rate to compare
    /// - Returns: Whether the rates are equal within Double precision
    private func matchesRate(_ unitsPerHour: Double) -> Bool {
        return abs(self.unitsPerHour - unitsPerHour) < .ulpOfOne
    }

    /// Determines whether the recommendation is necessary given the current state of the pump
    ///
    /// - Parameters:
    ///   - date: The date the recommendation would be delivered
    ///   - scheduledBasalRate: The scheduled basal rate at `date`
    ///   - lastTempBasal: The previously set temp basal
    ///   - continuationInterval: The duration of time before an ongoing temp basal should be continued with a new command
    ///   - scheduledBasalRateMatchesPump: A flag describing whether `scheduledBasalRate` matches the scheduled basal rate of the pump.
    ///                                    If `false` and the recommendation matches `scheduledBasalRate`, the temp will be recommended
    ///                                    at the scheduled basal rate rather than recommending no temp.
    /// - Returns: A temp basal recommendation
    func ifNecessary(
        at date: Date,
        scheduledBasalRate: Double,
        lastTempBasal: DoseEntry?,
        continuationInterval: TimeInterval,
        scheduledBasalRateMatchesPump: Bool
    ) -> TempBasalRecommendation? {
        // Adjust behavior for the currently active temp basal
        if let lastTempBasal = lastTempBasal,
            lastTempBasal.type == .tempBasal,
            lastTempBasal.endDate > date
        {
            /// If the last temp basal has the same rate, and has more than `continuationInterval` of time remaining, don't set a new temp
            if matchesRate(lastTempBasal.unitsPerHour),
                lastTempBasal.endDate.timeIntervalSince(date) > continuationInterval {
                return nil
            } else if matchesRate(scheduledBasalRate), scheduledBasalRateMatchesPump {
                // If our new temp matches the scheduled rate of the pump, cancel the current temp
                return .cancel
            }
        } else if matchesRate(scheduledBasalRate), scheduledBasalRateMatchesPump {
            // If we recommend the in-progress scheduled basal rate of the pump, do nothing
            return nil
        }

        return self
    }
}


/// Computes a total insulin amount necessary to correct a glucose differential at a given sensitivity
///
/// - Parameters:
///   - fromValue: The starting glucose value
///   - toValue: The desired glucose value
///   - effectedSensitivity: The sensitivity, in glucose-per-insulin-unit
/// - Returns: The insulin correction in units
private func insulinCorrectionUnits(fromValue: Double, toValue: Double, effectedSensitivity: Double) -> Double? {
    guard effectedSensitivity > 0 else {
        return nil
    }

    let glucoseCorrection = fromValue - toValue

    return glucoseCorrection / effectedSensitivity
}

/// Computes a target glucose value for a correction, at a given time during the insulin effect duration
///
/// - Parameters:
///   - percentEffectDuration: The percent of time elapsed of the insulin effect duration
///   - minValue: The minimum (starting) target value
///   - maxValue: The maximum (eventual) target value
/// - Returns: A target value somewhere between the minimum and maximum
private func targetGlucoseValue(percentEffectDuration: Double, minValue: Double, maxValue: Double) -> Double {
    // The inflection point in time: before it we use minValue, after it we linearly blend from minValue to maxValue
    let useMinValueUntilPercent = 0.5

    guard percentEffectDuration > useMinValueUntilPercent else {
        return minValue // if percentEffectDuration is < 50%, return the minValue (suspend threshold)
    }

    guard percentEffectDuration < 1 else {
        return maxValue // if percentEffectDuration is somehow > 100%, return the maxValue (average of correction range)
    }

    let slope = (maxValue - minValue) / (1 - useMinValueUntilPercent)
    return minValue + slope * (percentEffectDuration - useMinValueUntilPercent)
}


extension Collection where Element == GlucoseValue {

    /// For a collection of glucose prediction, determine the least amount of insulin delivered at
    /// `date` to correct the predicted glucose to the middle of `correctionRange` at the time of prediction.
    ///
    /// - Parameters:
    ///   - correctionRange: The schedule of glucose values used for correction
    ///   - date: The date the insulin correction is delivered
    ///   - suspendThreshold: The glucose value below which only suspension is returned
    ///   - sensitivity: The insulin sensitivity at the time of delivery
    ///   - model: The insulin effect model
    /// - Returns: A correction value in units, if one could be calculated                              // Isn't this comment wrong? This function returns an InsulinCorrection, not a correction value.
    private func insulinCorrection(
        to correctionRange: GlucoseRangeSchedule,
        at date: Date,
        suspendThreshold: HKQuantity,
        sensitivity: HKQuantity,
        model: InsulinModel
    ) -> InsulinCorrection? {
        var minGlucose: GlucoseValue?           // the prediction (type GlocuseValue) with the lowest value in the collection that is in the valid date range
        var eventualGlucose: GlucoseValue?      // the last prediction in the valid date range
        var correctingGlucose: GlucoseValue?    // of all the predicted GlucoseValues, this is the GlucoseValue that would require the least amount of insulin to move it to the "target" where target is described by func targetGlucoseValue()
        var minCorrectionUnits: Double?         // the units of insulin that would be required to move correctingGlucose to the "target" line.

        // Only consider predictions within the model's effect duration
        let validDateRange = DateInterval(start: date, duration: model.effectDuration)      // model.effectDuration is how long the insulin has effect on blood glucose values

        let unit = correctionRange.unit                                                     // unit (mg/dl or mmol) used to describe the correction range
        let sensitivityValue = sensitivity.doubleValue(for: unit)                           // ISF
        let suspendThresholdValue = suspendThreshold.doubleValue(for: unit)                 // suspend threshold

        // For each prediction above target, determine the amount of insulin necessary to correct glucose based on the modeled effectiveness of the insulin at that time
        // Original comment above says "for each prediction *above target*" - Didn't originally catch this, but this happens in the gaurd correctionUnits > 0
        for prediction in self {
            guard validDateRange.contains(prediction.startDate) else {
                continue
            }

            // If any predicted value is below the suspend threshold, return immediately
            guard prediction.quantity >= suspendThreshold else {
                return .suspend(min: prediction)
            }

            // Update range statistics
            if minGlucose == nil || prediction.quantity < minGlucose!.quantity {
                minGlucose = prediction                                                     // capture the prediction with the lowest glucose value
            }
            eventualGlucose = prediction                                                    // lazy way to capture the last prediction in the valid range

            let predictedGlucoseValue = prediction.quantity.doubleValue(for: unit)          // pull (Double) value out of prediction GlucoseValue
            let time = prediction.startDate.timeIntervalSince(date)                         // time = time since the insulin correction time (zero time)

            // Compute the target value as a function of time since the dose started
            let targetValue = targetGlucoseValue(
                percentEffectDuration: time / model.effectDuration,                                         // what percent of the way through the valid date range are we?
                minValue: suspendThresholdValue,                                                            // obvious
                maxValue: correctionRange.quantityRange(at: prediction.startDate).averageValue(for: unit)   // middle of the correction range
            )                                                                                               // if the %effectDuration < 50%, targetValue = suspendThreshold. Then linear from suspendThreshold to middle of correction range

            // Compute the dose required to bring this prediction to target:
            // dose = (Glucose Δ) / (% effect × sensitivity)

            let percentEffected = 1 - model.percentEffectRemaining(at: time)                // the percentEffectRemaining function returns the percentage of total insulin effect remaining at time time... (1 - percent effected) is something like the percentage of the insulin that would impact the BG at time time. Early on, very little of the insulin has had time to effect bg. By the end of the period, it has full effect.
            let effectedSensitivity = percentEffected * sensitivityValue                    // I think I would call this something else - it's ISF * the % of insulin that's been able to have impact.
            guard let correctionUnits = insulinCorrectionUnits(
                fromValue: predictedGlucoseValue,
                toValue: targetValue,
                effectedSensitivity: effectedSensitivity
                ), correctionUnits > 0 else {                                               // if correctionUnits < 0 (we're trying to raise BG to target line), don't do anything else with this prediction
                continue
            }

            // Update the correction only if we've found a new minimum
            // Note that the line above dictates that correctionUnits must be > 0
            guard minCorrectionUnits == nil || correctionUnits < minCorrectionUnits! else {
                continue
            }
            // This "min" is effectively determining the most insulin you can give at time "date" without causing future BG to go below the target line.
            // The definition of the target line still feels somewhat arbitraty to me.

            correctingGlucose = prediction                                                  // the GlucoseValue that corresponds to the minCorrection
            minCorrectionUnits = correctionUnits                                            // the smallest amount of insulin that will bring any one of the predictions to the target line
        }

        guard let eventual = eventualGlucose, let min = minGlucose else {
            return nil
        }

        // Choose either the minimum glucose or eventual glocse as the correction delta
        let minGlucoseTargets = correctionRange.quantityRange(at: min.startDate)            // get the correction range at the time of the minGlucose (which is now min)
        let eventualGlucoseTargets = correctionRange.quantityRange(at: eventual.startDate)  // get the correction range at the time of the eventualGlucose (which is now eventual)

        // Treat the mininum glucose when both are below range
        // If the minimum predicted glucose value and the eventual glucose value are both below the correction range at their respective times...
        if min.quantity < minGlucoseTargets.lowerBound &&
            eventual.quantity < eventualGlucoseTargets.lowerBound
        {
            let time = min.startDate.timeIntervalSince(date)
            // For time = 0, assume a small amount effected. This will result in large (negative) unit recommendation rather than no recommendation at all.
            let percentEffected = Swift.max(.ulpOfOne, 1 - model.percentEffectRemaining(at: time))      // I need to think about this more

            guard let units = insulinCorrectionUnits(
                fromValue: min.quantity.doubleValue(for: unit),                             // we're treating the min
                toValue: minGlucoseTargets.averageValue(for: unit),                         // same
                effectedSensitivity: sensitivityValue * percentEffected                     // as percentEffected goes to one, effectedSensitivity approaches ISF.
            ) else {
                return nil
            }

            return .entirelyBelowRange(
                correcting: min,                                                            // correcting means which predicted BG value are we using to calculate a correction - in this case, it's the min, not eventual
                minTarget: minGlucoseTargets.lowerBound,                                    // bottom of correction range at time of min predicted BG
                units: units                                                                //
            )
        } else if eventual.quantity > eventualGlucoseTargets.upperBound,                    // from the if that matches this else: if (minimum predicted BG OR eventual predicted BG is within or above correction range) AND from the else if on this line: eventual BG is above the correction range
            let minCorrectionUnits = minCorrectionUnits, let correctingGlucose = correctingGlucose
        {
            return .aboveRange(                                                             // .aboveRange means eventual BG is above range - other parts of the prediction could be below range
                min: min,                                                                   // just the min BG from the predictions
                correcting: correctingGlucose,                                              // this is the BG that corresponds to the minCorrection
                minTarget: eventualGlucoseTargets.lowerBound,                               // interesting that even though the correcting glucose corresponds to minCorrection, the minTarget is the lower bound of the correction range at the time of eventual BG
                units: minCorrectionUnits                                                   // the smallest amount of insulin that will bring any one of the predictions to the target line
            )
        } else {
            return .inRange
        }
    }

    /// Recommends a temporary basal rate to conform a glucose prediction timeline to a correction range
    ///
    /// Returns nil if the normal scheduled basal, or active temporary basal, is sufficient.
    ///
    /// - Parameters:
    ///   - correctionRange: The schedule of correction ranges
    ///   - date: The date at which the temp basal would be scheduled, defaults to now
    ///   - suspendThreshold: A glucose value causing a recommendation of no insulin if any prediction falls below
    ///   - sensitivity: The schedule of insulin sensitivities
    ///   - model: The insulin absorption model
    ///   - basalRates: The schedule of basal rates
    ///   - maxBasalRate: The maximum allowed basal rate
    ///   - lastTempBasal: The previously set temp basal
    ///   - rateRounder: Closure that rounds recommendation to nearest supported rate. If nil, no rounding is performed
    ///   - isBasalRateScheduleOverrideActive: A flag describing whether a basal rate schedule override is in progress
    ///   - duration: The duration of the temporary basal
    ///   - continuationInterval: The duration of time before an ongoing temp basal should be continued with a new command
    /// - Returns: The recommended temporary basal rate and duration
    func recommendedTempBasal(
        to correctionRange: GlucoseRangeSchedule,
        at date: Date = Date(),
        suspendThreshold: HKQuantity?,
        sensitivity: InsulinSensitivitySchedule,
        model: InsulinModel,
        basalRates: BasalRateSchedule,
        maxBasalRate: Double,
        lastTempBasal: DoseEntry?,
        rateRounder: ((Double) -> Double)? = nil,
        isBasalRateScheduleOverrideActive: Bool = false,
        duration: TimeInterval = .minutes(30),
        continuationInterval: TimeInterval = .minutes(11)
    ) -> TempBasalRecommendation? {
        let correction = self.insulinCorrection(
            to: correctionRange,
            at: date,
            suspendThreshold: suspendThreshold ?? correctionRange.quantityRange(at: date).lowerBound,
            sensitivity: sensitivity.quantity(at: date),
            model: model
        )

        let scheduledBasalRate = basalRates.value(at: date)
        var maxBasalRate = maxBasalRate

        // TODO: Allow `highBasalThreshold` to be a configurable setting
        if case .aboveRange(min: let min, correcting: _, minTarget: let highBasalThreshold, units: _)? = correction,
            min.quantity < highBasalThreshold
        {
            maxBasalRate = scheduledBasalRate
        }

        let temp = correction?.asTempBasal(
            scheduledBasalRate: scheduledBasalRate,
            maxBasalRate: maxBasalRate,
            duration: duration,
            rateRounder: rateRounder
        )

        return temp?.ifNecessary(
            at: date,
            scheduledBasalRate: scheduledBasalRate,
            lastTempBasal: lastTempBasal,
            continuationInterval: continuationInterval,
            scheduledBasalRateMatchesPump: !isBasalRateScheduleOverrideActive
        )
    }

    /// Recommends a bolus to conform a glucose prediction timeline to a correction range
    ///
    /// - Parameters:
    ///   - correctionRange: The schedule of correction ranges
    ///   - date: The date at which the bolus would apply, defaults to now
    ///   - suspendThreshold: A glucose value causing a recommendation of no insulin if any prediction falls below
    ///   - sensitivity: The schedule of insulin sensitivities
    ///   - model: The insulin absorption model
    ///   - pendingInsulin: The number of units expected to be delivered, but not yet reflected in the correction
    ///   - maxBolus: The maximum bolus to return
    ///   - volumeRounder: Closure that rounds recommendation to nearest supported bolus volume. If nil, no rounding is performed
    /// - Returns: A bolus recommendation
    func recommendedBolus(
        to correctionRange: GlucoseRangeSchedule,
        at date: Date = Date(),
        suspendThreshold: HKQuantity?,
        sensitivity: InsulinSensitivitySchedule,
        model: InsulinModel,
        pendingInsulin: Double,
        maxBolus: Double,
        volumeRounder: ((Double) -> Double)? = nil
    ) -> BolusRecommendation {
        guard let correction = self.insulinCorrection(
            to: correctionRange,
            at: date,
            suspendThreshold: HKQuantity(unit: HKUnit.milligramsPerDeciliter, doubleValue: 55.0),
            sensitivity: sensitivity.quantity(at: date),
            model: model
        ) else {
            return BolusRecommendation(amount: 0, pendingInsulin: pendingInsulin)
        }

        var bolus = correction.asBolus(
            pendingInsulin: pendingInsulin,
            maxBolus: maxBolus,
            volumeRounder: volumeRounder
        )

        // Handle the "current BG below target" notice here
        // TODO: Don't assume in the future that the first item in the array is current BG
        if case .predictedGlucoseBelowTarget? = bolus.notice,
            let first = first, first.quantity < correctionRange.quantityRange(at: first.startDate).lowerBound
        {
            bolus.notice = .currentGlucoseBelowTarget(glucose: first)
        }

        return bolus
    }
}
